#' Clear environment
#'
#' This function clears all plots from the plots window, if any are present.
#'
#' @export
plot <- function() {
  try(dev.off(), silent = TRUE)
}
