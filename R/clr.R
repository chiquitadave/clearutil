#' Clear
#'
#' This function clears several optional areas of RStudio.
#'
#' @param ... up to 4 arguments, which may include console, environment,
#' packages, and plot. Truncated versions of these work as well, down to
#' co, en, pa, and pl, respectively. There is an optional argument, all, which
#' will clear all 4. Inputting no arguments will clear the
#' console, global environment, and plots. \code{clr(...)} will read all
#' arguments independent of order and truncation.
#' @examples
#' \code{clr()}
#' \code{clr(environment)}
#' \code{clr(en)}
#' \code{clr(pack, pl, environment)}
#' \code{clr(all)}
#' @export
clr <- function(...) {
  args <- sapply(substitute(list(...)), deparse)[-1]
  if (length(args) > 0) {
    for (i in c(1:length(args))) {
      if (grepl("co", args[[i]]) == FALSE) {
        if (grepl("en", args[[i]]) == FALSE) {
          if (grepl("pa", args[[i]]) == FALSE) {
            if (grepl("pl", args[[i]]) == FALSE) {
              if (grepl("all", args[[i]]) == FALSE) {
                return(warning(paste("Argument", i, "was unrecognized. The only valid arguments for clr() are 'console', 'environment', 'packages', 'plot', and 'all'. For help, read the clr() documentation at ?clearutil::clr", sep = " ")))
              }
            }
          }
        }
      }
    }
    if (length(args) > 4) {
      return(warning("Too many arguments! clr() only supports 4 arguments. For help, read the clr() documentation at ?clearutil::clr"))
    }
    if (length(grep("all", args))) {
      if (length(args) > 1) {
        return(warning("Too many arguments! Other arguments are not necessary when 'all' is used. For help, read the clr() documentation at ?clearutil::clr"))
      }
      if (grep("all", args)) {
        args <- list("console", "environment", "plot", "packages")
      }
    }
    args[grep("co", args)] <- "console"
    args[grep("en", args)] <- "environment"
    args[grep("pa", args)] <- "packages"
    args[grep("pl", args)] <- "plot"
    if (anyDuplicated(args)) {
      return(warning("Duplicate arguments! It is not necessary to input the same argument more than once. Truncated and non-truncated arguments are treated identically. For help, read the clr() documentation at ?clearutil::clr"))
    }
  } else {
    args <- list("console", "environment", "plot")
  }
  if ("environment" %in% args) {  # Clear environment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  }
  if ("plot" %in% args) {
    if (!is.null(dev.list())) dev.off()  # Clear plots
  }
  if ("packages" %in% args) {  # Clear packages
    packages <- names(sessionInfo()$otherPkgs)
    if (is.null(packages)) {
      cat("No packages loaded")
    } else {
      suppressWarnings(
        invisible(
          lapply(
            paste0("package:", packages),
            detach,
            character.only = TRUE,
            unload = TRUE,
            force = TRUE
          )
        )
      )
      cat(paste0("Packages successfully unloaded: ", toString(packages)))
    }
  }
  if ("console" %in% args) cat("\f") # Clear console
}
