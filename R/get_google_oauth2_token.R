#' get_google_oauth2_token
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' @import httr
#'
get_google_oauth2_token <- function(){

  #One needs to set these up in the google cloud console https://developers.google.com/fit/rest/v1/get-started
  fitness_store <- oauth_app(
    "google",
    google_fit_key(),
    google_fit_secret()
  )

  google_token <-
    oauth2.0_token(endpoint = oauth_endpoints("google"),
                   app = fitness_store,
                   scope = "https://www.googleapis.com/auth/fitness.activity.read",
                   use_oob = TRUE,
                   cache = TRUE)
}

google_fit_key <- function() {
  key <- Sys.getenv('GOOGLE_FIT_KEY')
  if (identical(key, "")) {
    stop("Please set env var GOOGLE_FIT_KEY to a Google OAuth Client key",
         call. = FALSE)
  }

  key
}

google_fit_secret <- function() {
  secret <- Sys.getenv('GOOGLE_FIT_SECRET')
  if (identical(secret, "")) {
    stop("Please set env var GOOGLE_FIT_SECRET to a Google OAuth Client key",
         call. = FALSE)
  }

  secret
}
