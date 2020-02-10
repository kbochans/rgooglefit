#' get_google_oauth2_token
#'
#' Authorize access to Google Fitness Store. Access token will be cached
#' by default.
#'
#'
#' @param cache Should token be stored locally? Defaults to TRUE.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' #First set up client id and secret in
#' # google cloud console
#' #https://developers.google.com/fit/rest/v1/get-started
#'
#' fitness_store_auth()
#'
#' #add some subsequent steps
#'}
#'
#' @import httr
#'
fitness_store_auth <- function(cache = TRUE){

  fitness_store <- oauth_app(
    "google",
    check_var("GOOGLE_FIT_KEY"),
    check_var("GOOGLE_FIT_SECRET")
  )

  token <-
    oauth2.0_token(endpoint = oauth_endpoints("google"),
                   app = fitness_store,
                   scope = "https://www.googleapis.com/auth/fitness.activity.read",
                   use_oob = TRUE,
                   cache = cache)

  return(token)
}

