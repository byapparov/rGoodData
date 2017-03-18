#' @import jsonlite
#' @import httr

library(httr)
library(jsonlite)

#' Gets the SST (SuperSecured Token) for Login
#'
#'
#' @export
#' @return STT
superSecuredToken <- function() {
  values <- toJSON(list(postUserLogin = list(login = Sys.getenv("GOODDATA_USER"),
                                             password = Sys.getenv("GOODDATA_PASSWORD"),
                                             remember = 1,
                                             verify_level = 0)),
                   pretty = T,
                   auto_unbox = T)

  response = POST(url = paste0(Sys.getenv("GOODDATA_DOMAIN"), "/gdc/account/login/"),
           body = values,
           add_headers("Content-Type" = "application/json",
                        Accept = "application/json"),
           encode = "json")

  processResponse(response)

  sst = response$headers$`set-cookie`
  return(sst)
}

#' Gets the TT (Temporary Token) for a given SST
#' @export
#' @param sst SST
#' @return TT
temporaryToken <- function(sst) {
  response <- GET(paste0(Sys.getenv("GOODDATA_DOMAIN"), "/gdc/account/token"),
                 add_headers(
                   'Accept' = 'application/json',
                   'Content-Type' = 'application/json',
                   'Cookie' =  sst)
              )

  processResponse(response)

  tt <- response$headers$`set-cookie`
  return(tt)
}

#' Gets that TT auth cookie, wrapper for the easy access to TT
#'
#' @export
#' @return TT
authCookie <- function() {
  sst <- superSecuredToken()
  tt <- temporaryToken(sst)
  return(tt)
}
