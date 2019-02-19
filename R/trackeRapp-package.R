#' @import trackeR
#' @import shiny
#' @import plotly

## #' @importFrom plotly plotlyProxy plotlyProxyInvoke plotlyOutput subplot event_data plot_ly layout config style event_data plot_mapbox add_paths add_markers add_segments add_lines add_bars %>% renderPlotly
#'
#' @importFrom grDevices colorRampPalette
#'
#' @importFrom shinyWidgets updatePickerInput dropdownButton tooltipOptions pickerInput actionBttn checkboxGroupButtons awesomeRadio updateCheckboxGroupButtons
#'
#' @importFrom shinyjs hidden disable show hide enable html click delay useShinyjs extendShinyjs runjs js addClass
#'
#' @importFrom shinyalert useShinyalert shinyalert
#'
#' @importFrom shinydashboard box dashboardPage dashboardHeader dashboardSidebar sidebarMenu dashboardBody valueBox valueBoxOutput renderValueBox
#'
#' @importFrom shinycssloaders withSpinner
#'
#' @importFrom zoo index coredata
#'
#' @importFrom changepoint cpt.mean cpts coef
#'
#' @importFrom mgcv gam predict.gam
#'
#' @importFrom stats sd na.omit median
#'
#' @importFrom utils read.csv
#'
#' @importFrom foreach getDoParWorkers foreach %dopar% %do%
#'


if (getRversion() >= "2.15.1") globalVariables(c("session"))
