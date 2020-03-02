#' Fitness API
#'
#' @param path An endpoint to specifically query. See \url{https://developers.google.com/fit/rest}
#' @param token A token from \code{\link{fitness_store_auth}}
#'
#' @return A response object
#' @export
#'
#'@examples
#'\dontrun{
#' res <- fitness_api("fitness/v1/users/me/sessions", google_token)
#'}
#'
fitness_api <- function(path,token) {
  url <- modify_url("https://www.googleapis.com", path = path)

  resp <- GET(url,token)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "fitness_api"
  )
}

print.fitness_api <- function(x, ...) {
  cat("<Fitness ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}
