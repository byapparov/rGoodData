library(httr)
library(data.table)

# Environment variables required:
# GOODDATA_DOMAIN
# GOODDATA_PROJECT
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

  api.url <- paste0(Sys.getenv("GOODDATA_DOMAIN"), "/gdc/projects/",
                    Sys.getenv("GOODDATA_PROJECT"), "/execute/raw")

  response <- POST(url = api.url,
                   body = values,
                   authenticate(Sys.getenv("GOODDATA_USER"), Sys.getenv("GOODDATA_PASSWORD")),
                   content_type_json() ,
                   add_headers(Accept = "application/json",
                               "Content-Type" = "application/json"))
  c <- content(response, "parsed", "application/json")
  uri <- paste0(Sys.getenv("GOODDATA_DOMAIN"), c$uri)
  return(uri)
}


#' Gets report results as data.table
#' @export
#'
#' @param uri url for the report resutls
#' @return data.table with report
getReportData <- function(uri) {
  response <- GET(uri,
                  authenticate(Sys.getenv("GOODDATA_USER"), Sys.getenv("GOODDATA_PASSWORD")),
                  add_headers(Accept = "application/json",
                              "Content-Type" = "application/json"))
  report <- content(response, "text", encoding = "UTF-8")
  report <- fread(report, encoding = "UTF-8")
  return(report)
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
  c <- content(response, "parsed", "application/json")
  last.definition <- tail(c$report$content$definitions, 1)[[1]]
  definition.obj <- tail(strsplit(last.definition, "/")[[1]], 1)
  return(as.integer(definition.obj))
}
