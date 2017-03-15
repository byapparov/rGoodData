library(httr)
library(data.table)

# Environment variables required:
# GOODDATA_DOMAIN - this is what goes before /gdc/ in your GoodData account
# GOODDATA_PROJECT - this is the hash of the project that can be found in the url
# GOODDATA_USER
# GOODDATA_PASSWORD

#' Function gets uri for export of raw report data
#' @export
#'
#' @param obj integer id of the report definition, which refers to a version of a report
#' @return uri of the report results that can be uploaded with the GET request
getReportRawUri <- function(obj) {
  values = sprintf('
                  {
                    "report_req": {
                      "reportDefinition": "/gdc/md/%1$s/obj/%2$i"
                    }
                  }', Sys.getenv("GOODDATA_PROJECT"), obj)

  api.url <- paste0(Sys.getenv("GOODDATA_DOMAIN"), "/gdc/app/projects/",
                    Sys.getenv("GOODDATA_PROJECT"), "/execute/raw")

  response <- POST(url = api.url,
                   body = values,
                   authenticate(Sys.getenv("GOODDATA_USER"), Sys.getenv("GOODDATA_PASSWORD")),
                   content_type_json() ,
                   add_headers(Accept = "application/json",
                               "Content-Type" = "application/json"))

  c <- processResponse(response)
  uri <- paste0(Sys.getenv("GOODDATA_DOMAIN"), c$uri)
  return(uri)
}


#' Gets report results as data.table
#' @export
#'
#' @param uri url for the report resutls
#' @param wait time to wait till next get attempt if report is not ready
#' @return data.table with report
getReportData <- function(uri, wait = 5) {
  response <- GET(uri,
                  authenticate(Sys.getenv("GOODDATA_USER"), Sys.getenv("GOODDATA_PASSWORD")),
                  add_headers(Accept = "application/json",
                              "Content-Type" = "application/json"))
  status <- status_code(response)
  if(status >= 200 & status < 300) {
    if(getResponseType(response) == "text/csv") {
      report <- content(response, "text", encoding = "UTF-8")
      report <- fread(report, encoding = "UTF-8")
    } else { # we have to retry as report data was not generated yet.
      cat("\rWaiting for report: ", wait, " sec")
      Sys.sleep(wait)
      wait <- min(wait * 2, 60)
      report <- getReportData(uri, wait)
    }
    return(report)
  } else { # error
    type <- http_type(response)
    if (type == "application/json") {
      out <- content(response, "parsed", "application/json")
      stop("HTTP error [", out$error$code, "] ", out$message, call. = FALSE)
    } else {
      out <- content(response, "text")
      stop("HTTP error [", status, "] ", out, call. = FALSE)
    }
  }
}

#' Gets last definition for a report object id
#' @export
#'
#' @param report.obj integer id of the report object
#' @return integer id of the latest report definitin object
getLastDefinition <- function(report.obj) {
  response <- GET(url = paste0(Sys.getenv("GOODDATA_DOMAIN"), "/gdc/md/", Sys.getenv("GOODDATA_PROJECT"), "/obj/", report.obj),
                  authenticate(Sys.getenv("GOODDATA_USER"), Sys.getenv("GOODDATA_PASSWORD")),
                  add_headers("Accept" = "application/json",
                              "Content-Type" = "application/json"))

  c <- processResponse(response)
  last.definition <- tail(c$report$content$definitions, 1)[[1]]
  definition.obj <- tail(strsplit(last.definition, "/")[[1]], 1)
  return(as.integer(definition.obj))
}

#' Processes http response and returns a list of results
#'
#' @param response http response from the GoodData API
#' @return list of resutls parsed from the response
processResponse <- function(response) {
  if(status_code(response) == 200) {
    c <- content(response, "parsed", http_type(response))
  } else { # error
    type <- http_type(response)
    if (type == "application/json") {
      out <- content(response, "parsed", "application/json")
      stop("HTTP error [", out$error$code, "] ", out$error$message, call. = FALSE)
    } else {
      out <- content(response, "text")
      stop("HTTP error [", response$status, "] ", out, call. = FALSE)
    }
  }
}

getResponseType <-function(response) {
  return(response$headers$`content-type`)
}
