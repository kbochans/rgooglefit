#' Check Variables
#'
#' @param environmental_variable An environmental variable to check for.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_var("GOOGLE_FIT_KEY"),
#' }
#'
check_var<- function(environmental_variable) {
  var <- Sys.getenv(environmental_variable)
  if (identical(var, "")) {
    stop(sprintf("Please set env var",environmental_variable,"to a Google OAuth Client key"),
         call. = FALSE)
  }

  var
}

#' Token Check
#'
#' Check to see if .httr-oauth file exists
#'
#'
token_check <- function() {
  path <- ".httr-oauth"
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  invisible("Token Exists")
}

#' Milliseconds to Datetime
#'
#' @param df Provide a dataframe from \code{\link{fitness_api}}
#'
#' @export
#'
#'@import dplyr
milliseconds_to_datetime <- function(df){
  df %>%
    mutate_at(
      vars(contains('Millis'),-.data$activeTimeMillis),
      ~ as.numeric(.) %>% lubridate::dmilliseconds() %>% lubridate::as_datetime()
    ) %>%
    mutate_at(vars(.data$activeTimeMillis),~as.numeric(.) %>% lubridate::dmilliseconds()) %>%
    rename_at(vars(contains('Millis')),  ~ stringr::str_remove(., "Millis"))
}
