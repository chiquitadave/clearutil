#' Clear environment
#'
#' This function clears all variables from the global environment.
#'
#' @export
env <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}
