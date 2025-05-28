shinypal_env <- new.env()
shinypal_env$setup <- FALSE

set_var <- function(name, value) {
  assign(name, value, envir = shinypal_env)
}

get_var <- function(name) {
  if (exists(name, envir = shinypal_env)) {
    get(name, envir = shinypal_env)
  } else {
    NULL
  }
}

#' @keywords internal
check_setup <- function() {
  if (shinypal_env$setup == FALSE) {
    stop("shinypal has not been set up. Please run shinypal_setup() first.",
         call. = FALSE)
  }
}
