#' Launch the \pkg{trackeRapp} interface
#'
#' \pkg{trackeRapp} provides an integrated dashboard and workflow for
#' the analysis of running, cycling and swimming data from GPS-enabled tracking
#' devices through the \code{trackeR} R package. \code{trackerRapp} or
#' \code{trackeR_app} launches the interface.
#'
#' @param quiet If \code{TRUE} (default), then warnings and errors
#'     while using the interface are printed in standard output. If
#'     \code{FALSE}, then all warnings and errors are suppressed.
#'
#' @section Getting started:
#' Once the interface launches, you may
#' experiment with the interface by hitting "Load" and then
#' "Upload sample dataset".
#'
#' See the \code{"tour de trackeRapp"} pages at
#' \url{https://trackerproject.github.io/trackeRapp/} for tutorial
#' videos, explanation of the workflow and visualizations that
#' \pkg{trackeRapp} offers, and to, generally, learn more about
#' \pkg{trackeRapp} and all of its capabilities.
#' 
#'
#' @section Video channel:
#' \pkg{trackeRapp} has a dedicated YouTube channel at
#' \url{https://www.youtube.com/channel/UCY6y-pw8d1kek1WAIWiVhhw}. The
#' channel features video tutorials about \pkg{trackeRapp} and the
#' workflow it provides.
#'
#' @section Development notes and acknowledgements:
#' \code{trackeRapp} has been designed and developed by Robin Hornak
#' and Ioannis Kosmidis, while Robin Hornak was completing his
#' undergraduate research project in the Department of Statistical
#' Science, University College London under the supervision of Ioannis
#' Kosmidis. Ioannis Kosmidis has been supported by the Alan Turing
#' Institute under the EPSRC grant EP/N510129/1 (Turing award number
#' TU/B/000082) and University of Warwick. Robin Hornak and Ioannis
#' Kosmidis have also been supported by University of Warwick through
#' a Warwick Impact Fund Award that runs from May 2018 to December
#' 2019. The support of the aforementioned organizations is greatly
#' acknowledged.
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
#' @examples
#'
#' if (interactive()) {
#'   trackeRapp(quiet = TRUE)
#' }
#'
#' if (interactive()) {
#'   trackeR_app(quiet = FALSE)
#' }
#'
#' # Experiment with the interface by hitting "Load" and then
#' # "Upload sample dataset".
#'
#' @import trackeR
#' @import shiny
#' @import plotly
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_centroid st_linestring st_multilinestring st_sf st_sfc
#' @importFrom shinyWidgets updatePickerInput dropdownButton tooltipOptions pickerInput actionBttn checkboxGroupButtons awesomeRadio updateCheckboxGroupButtons
#' @importFrom shinyjs hidden disable show hide enable html click delay useShinyjs extendShinyjs runjs js addClass
#' @importFrom shinydashboard box dashboardPage dashboardHeader dashboardSidebar sidebarMenu dashboardBody valueBox valueBoxOutput renderValueBox
#' @importFrom zoo zoo index coredata rollmedian
#' @importFrom changepoint cpt.mean cpts coef
#' @importFrom mgcv gam predict.gam
#' @importFrom stats sd na.omit median
#' @importFrom utils read.csv
#' @importFrom foreach getDoParWorkers foreach %dopar% %do%
#' @importFrom DT renderDT datatable DTOutput dataTableProxy selectRows JS formatStyle
#'
#' @export
trackeRapp <- function(quiet = TRUE) {
    if (isTRUE(quiet)) {
        suppressWarnings(runApp(appDir = system.file('shiny', package = 'trackeRapp'),
                                launch.browser = TRUE, quiet = TRUE))
    }
    else {
        runApp(appDir = system.file('shiny', package = 'trackeRapp'),
               launch.browser = TRUE, quiet = FALSE)
    }
}

#' @rdname trackeRapp
#' @export
trackeR_app <- trackeRapp


## Define global variables
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("Series", "session", "variable", "type"))
}
