#' Get Sessions
#'
#' Get the time interval during which users perform a fitness activity.
#' More info at \url{https://developers.google.com/fit/rest/v1/using-sessions}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_sessions
#' }
#'
get_sessions <- function() {

  #get token
  google_token <- fitness_store_auth()

  #send get request to sessions endpoint
  res <- fitness_api("fitness/v1/users/me/sessions", google_token)

  #turn content into a useful dataframe
  res$content$session %>%
    purrr::map(tibble::as.tibble) %>%
    dplyr::bind_rows() %>%
    milliseconds_to_datetime()
}
