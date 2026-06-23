shinypal_env <- new.env()
shinypal_env$setup <- FALSE

#' @keywords internal
check_setup <- function() {
  if (shinypal_env$setup == FALSE) {
    stop("shinypal has not been set up. Please run shinypal_setup() first.",
         call. = FALSE)
  }
}
