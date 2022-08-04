.onAttach <- function(libname, pkgname) {
  packageStartupMessage("cavityuse v", utils::packageVersion("cavityuse"), "\n",
                        "Please note that 'cavityuse' is still in early ",
                        "development\n",
                        "Help by submitting bugs/feature requests: ",
                        "http://github.com/steffilazerte/cavityuse/issues")
}

#' An R package for detecting cavity use from geolocator data
#'
#' \code{cavityuse} is an R package for detecting use of cavities (caves,
#' burrows, etc.) from geolocator data in a variety of animal species.
#'
#'
#' There are two main aspects of this package:
#'
#' \enumerate{
#'   \item Detect patterns of cavity use in your data
#'   \itemize{
#'     \item \code{\link{sun_detect}()} Detect sunrise/sunset events in your
#'     data
#'     \item \code{\link{cavity_detect}()} Detect patterns of cavity use in your
#'     data (sunrise/sunset times from \code{\link{sun_detect}()} are used to
#'     detect cavity use at night).
#'     \item \code{\link{cavity_split}()} Split bouts of cavity use according
#'     the time frame you're interested in (i.e., midnight, noon, sunrise/sunset)
#'     }
#'
#'  \item Plot patterns of cavity use
#'  \itemize{
#'    \item \code{\link{cavity_plot}()}
#'    }
#'  }
#'
#' We also include several practice data sets:
#'
#'  \itemize{
#'    \item \code{\link{flicker}}
#'    \item \code{\link{flicker_mult}}
#'    \item \code{\link{wtsp}}
#'    \item \code{\link{calib}}
#'    }
#'
#'
#'
#' @docType package
#' @name cavityuse-package
#' @aliases cavityuse cavityuse-package
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data
NULL

# Dealing with CRAN Notes due to Non-standard evaluation
.onLoad <- function(libname = find.package("weathercan"),
                    pkgname = "weathercan"){
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to
      # avoid CRAN warnings
      c(".") # piping requires '.' at times
    )
  invisible()
}
