## #' Plot an object of class \code{trackeRdataSummary}.
## #'
## #' @param data An object of class \code{reactiveValues} having elements at least \code{object} (a \code{trackeRdata} object), \code{summary} (a \code{trackeRdataSummary} object), and \code{selected_sessions}.
## #' @param what Name of variables to be plotted.
## #' @param date Should the date or the session number be used on the abscissa?
## #' @param summary_type Which type of variables should be plotted? This can either be
## #'     \code{"total"}, \code{"moving"}, \code{"resting"}. Default is \code{"total"}.
## #' @param lines Should interpolating lines be plotted?
## #' @param options List of plotting options
## #' @seealso \code{\link{summary.trackeRdata}}
plot_workouts <- function(data,
                          what,
                          summary_type = "total",
                          date = TRUE,
                          lines = TRUE,
                          options = NULL) {
    opts <- if (is.null(options)) trops() else options

    so <- data$summary
    sessions <- so$session

    ## FEATURE: Add ability to add moving/resting plots
    ## if (what %in% c('distance', 'duration', 'wrRatio')) {
    ##     summary_type <- "total"
    ## }
    ## else {
    ##     summary_type <- "total"
    ## }

    ## FEATURE: Add moving average masks

    ## subset
    so <- so[sessions]

    feature <- lab_sum(feature = what, data = so, whole_text = TRUE)
    units_text <- lab_sum(feature = what, data = so, whole_text = FALSE)

    nsessions <- length(unique(so$session))
    ndates <- length(unique(so$sessionStart))

    units <- get_units(so)

    ## subsets on variables and type
    so <- fortify_trackeRdataSummary(so, melt = TRUE)
    so <- subset(so, variable == what & type == summary_type)

    so$sport <- data$summary$sport
    so$col <- ifelse(so$sport == "running",
                     opts$summary_plots_selected_colour_run,
              ifelse(so$sport == "cycling",
                     opts$summary_plots_selected_colour_ride,
                     opts$summary_plots_selected_colour_swim))


    ## remove empty factor levels
    so <- droplevels(so)

    ## clean up: if there are only NA observations for a variable, the (free) y-scale cannot
    ## be determined
    empty <- tapply(so$value, so$variable, function(x) all(is.na(x)))
    if (any(empty)) so <- subset(so, !(variable %in% names(empty)[empty]))

    ## single session
    if (nsessions < 2) {
        so$sessionStart <- format(so$sessionStart, format = "%Y-%m-%d")
        so <- droplevels(so)
    }

    ## x axis
    if (date) {
        so$xaxis <- so$sessionStart
        xlab <- "Date"
    }
    else {
        so$xaxis <- so$session
        xlab <- "Session"
    }

    p <- plot_ly(so,
                 x = ~ xaxis, y = ~ value, hoverinfo = "text",
                 text = ~ paste(" Session:", session, "\n",
                                "Date:", format(sessionStart, format = "%Y-%m-%d"),
                                "\n", convert_to_name(what), ":", round(value, 2), units_text, "\n",
                                "Sport:", sport), showlegend = FALSE) %>%
        add_markers(key = so$session, color = I(opts$summary_plots_deselected_colour),
                    symbol = ~ sport,
                    symbols = c("circle", "x", "square"),
                    legendgroup = ~ sport,
                    showlegend = TRUE) %>%
        add_lines(color = I(opts$summary_plots_deselected_colour), connectgaps = TRUE, legendgroup = ~ sport,
                  line = list(shape = "spline", smoothing = 0.5, showlegend = FALSE))


    ## selected sesssions
    so_sub <- so[so$session %in% data$selected_sessions, ]
    p <- add_markers(p,
                     data = so_sub,
                     color = I(so_sub$col), #I(opts$summary_plots_selected_colour),
                     symbol = ~ sport,
                     symbols = c("circle", "x", "square"),
                     showlegend = FALSE)

    range_x <- c(min(so$xaxis), max(so$xaxis))

    if (nsessions > 1) {
        range_x[2] <- range_x[2] + 0.01 * diff(range_x)
        range_x[1] <- range_x[1] - 0.01 * diff(range_x)
    }

    low_factor <- opts$summart_plots_yaxis_min_factor
    upp_factor <- opts$summart_plots_yaxis_max_factor
    range_y <- function(feature, data) {
        upp <- max(so$value, na.rm = TRUE)
        upp <- ifelse(upp > 0, upp * upp_factor, upp * low_factor)
        if (feature == 'avgHeartRate') {
            low <- 50
        }
        else {
            if (feature %in% c('avgSpeed', 'avgPace', 'avgCadenceCycling', 'avgCadenceRunning', 'avgTemperature', 'avgAltitude')) {
                low <- min(so$value, na.rm = TRUE)
                low <- ifelse(low > 0, low * low_factor, low * upp_factor)
            }
            else {
                low <- 0
            }
        }
        c(low, upp)
    }

    ## Axis lists
    y <- list(title = units_text,
              range = range_y(what, so))
    if (what == "avgPace") {
        y$range <- y$range[c(2,1)]
    }
    x <- list(title = "",
              range = range_x)


    layout(p,
           dragmode = "select", showlegend = TRUE, yaxis = y, legend = list(y = 1.1, orientation = "h"),
           xaxis = x, margin = list(l = 80, b = 50, pad = 0),
           plot_bgcolor = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
        ## for list of modebar buttons, see
        ## https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = list("zoomIn2d",
                                             "zoomOut2d",
                                             "autoScale2d",
                                             "toggleSpikelines",
                                             "hoverClosestCartesian",
                                             "hoverCompareCartesian"))
}
