
#' Execute schedule
#'
#' Executes GoodData schedule
#'
#' @param schedule.id The ID of the schedule you want to manage.
#' @param retry  If true, applies the reschedule property if the schedule has it set. When not set, defaults to false.
#' @return uri link to schedule execution.
#'
#' @seealso
#'   \href{https://help.gooddata.com/display/doc/API+Reference#/reference/scheduling-and-notifications/manage-schedule-executions/execute-a-schedule}{GoodData API Documentation - Execute Schedule}
executeSchedule <- function(schedule.id, retry = FALSE) {

  values.list <- list(
    execution = list(
      params = list(
        retry = ifelse(retry, "true", "false")
      )
    )
  )
  values <- toJSON(values.list, auto_unbox = T, pretty = T)

  api.url <- paste0(Sys.getenv("GOODDATA_DOMAIN"), "/gdc/projects/",
                    Sys.getenv("GOODDATA_PROJECT"), "/schedules/", schedule.id, "/executions")

  response <- POST(url = api.url,
                   body = values,
                   content_type_json() ,
                   add_headers(Accept = "application/json",
                               "Content-Type" = "application/json",
                               "User-Agent" = getUserAgent(),
                               Cookie = authCookie()))

  c <- processResponse(response)
  uri <- paste0(Sys.getenv("GOODDATA_DOMAIN"), c$execution$links$self)
  return(uri)
}
