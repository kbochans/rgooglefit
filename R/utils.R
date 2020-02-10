check_var<- function(environmental_variable) {
  var <- Sys.getenv(environmental_variable)
  if (identical(var, "")) {
    stop(sprintf("Please set env var",environmental_variable,"to a Google OAuth Client key"),
         call. = FALSE)
  }

  var
}
