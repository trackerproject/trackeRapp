#' trackeRapp: Interface for the analysis of running and cycling data
#' from GPS-enabled tracking devices.
#'
#' 'trackeRapp' provides an integrated dashboard and workflow for the
#' analysis of running and cycling data from GPS-enabled tracking
#' devices through the 'trackeR' R package.
#'
#' @section Launching the app:
#'
#' The interface can be launched by typing \code{\link{trackeR_app}}
#' or \code{\link{trackeRapp}}.
#'
#' @note
#'
#' 'trackeRapp' has been designed and developed by Robin Hornak and
#' Ioannis Kosmidis, while Robin Hornak was completing his
#' undergraduate research project in the Department of Statistical
#' Science, University College London under the supervision of Ioannis
#' Kosmidis. Ioannis Kosmidis has been supported by the Alan Turing
#' Institute under the EPSRC grant EP/N510129/1 (Turing award number
#' TU/B/000082) and Univeristy of Warwick. Robin Hornak and Ioannis
#' Kosmidis have also been supported by University of Warwick through
#' a Warwick Impact Fund Award that runs from May 2018 to May
#' 2019. The support of the aforementioned organisations is greatly
#' acknowledged.
#'
#'
#' @references
#'
#' Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
#' and Cycling Data from GPS-Enabled Tracking Devices in
#' R. \emph{Journal of Statistical Software}, \bold{82}(7),
#' 1--29. doi:10.18637/jss.v082.i07
#'
#' Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#' Endurance Runners to Training and Physiological Effects via
#' Multi-Resolution Elastic Net. \emph{ArXiv e-print}
#' arXiv:1506.01388.
#'
#' @docType package
#' @name trackeRapp
#' @import trackeR
#' @import shiny
#' @importFrom stats na.omit median
#' @importFrom utils read.csv
#' @importFrom plotly "%>%"
NULL


## Define global variables
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("Series"))  # plot_selectedWorkouts
}

#' Launch the trackeR dashboard
#'
#' @inheritParams shiny::runApp
#'
#' @export
trackeRapp <- function(quiet = FALSE) {
    shiny::runApp(appDir = system.file('shiny', package = 'trackeRapp'),
                  launch.browser = TRUE, quiet = quiet)
}

#' @rdname trackeRapp
#' @export
trackeR_app <- trackeRapp
