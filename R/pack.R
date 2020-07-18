#' Clear environment
#'
#' This function unloads all packages, if any are present.
#'
#' @export
pack <- function() {
  try(pacman::p_unload(all), silent = TRUE)
}
