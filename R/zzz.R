# Fetch this session's shinypal state (session$userData$shinypal), raising a
# clear error if shinypal_setup() hasn't run yet
#' @keywords internal
check_setup <- function() {
  env <- shiny::getDefaultReactiveDomain()$userData$shinypal
  if (is.null(env) || !isTRUE(env$setup)) {
    stop("shinypal has not been set up. Please run shinypal_setup() first.",
         call. = FALSE)
  }
  env
}
