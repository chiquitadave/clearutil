#' Clear
#'
#' This function clears several optional areas of RStudio. If the function has
#' no arguments, it will by default clear the Global Environment, Plots
#' window, and the Console window.
#'
#' @export
clr <- function(...) {
  args <- rlang::exprs(...)
  if (length(args) == 0) {
    clearutil::env()
    clearutil::plot()
    clearutil::cons()
  } else {
    for (i in c(1:length(args))) {
      eval(substitute(clearutil::x(), list(x = args[[i]])), envir = .GlobalEnv)
    }
  }
}
