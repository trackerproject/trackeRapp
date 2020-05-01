## #' A timeline plot for workouts.
## #'
## #' @param sumX An object of class \code{trackeRdataSummary}.
## #' @param plotly Logical. Return plotly plots or standard trackeR plots
## #' @param shiny Logical. Whether plots are in a shiny environment.
## #' @param session A vector. Selected session numbers.
plot_timeline <- function(sumX, session, shiny=TRUE, plotly=TRUE, options = NULL) {
    opts <- if (is.null(options)) trops() else options
    if (plotly) {
        d <- if (shiny) event_data("plotly_selected") else NULL
        startdates <- as.Date(sumX$sessionStart)
        enddates <- as.Date(sumX$sessionEnd)
        ## Hack to extract times
        endtimes <- as.POSIXct(paste(Sys.Date(), format(sumX$sessionStart, "%H:%M:%S")))
        starttimes <- as.POSIXct(paste(Sys.Date(), format(sumX$sessionEnd, "%H:%M:%S")))
        df <- data.frame(sday = startdates, eday = enddates, start = starttimes, end = endtimes, session = sumX$session, sport = sumX$sport)
        p <- plot_ly()
        p <- add_markers(p, data = df,
                         x = ~ start, y = ~ sday,
                         key = ~ session, alpha = 0,
                         hoverinfo = "none")
        p <- add_segments(p,
                          data = df, x = ~ start, xend = ~ end, y = ~ sday, yend = ~ eday,
                          color = I(opts$summary_plots_deselected_colour),
                          size = I(3),
                          alpha = 0.5,
                          hoverinfo = "text",
                          text = sprintf("Session: %s<br>Start: %s <br>End: %s",
                                         df$session, sumX$sessionStart, sumX$sessionEnd))

        all_sessions <- nrow(sumX)
        ## Three different traces to allow for different colours
        for (sp in c("swimming", "cycling", "running")) {
            df_sub <- df[df$session %in% session & df$sport == sp, ]
            col <- ifelse(sp == "running",
                          opts$summary_plots_selected_colour_run,
                   ifelse(sp == "cycling",
                          opts$summary_plots_selected_colour_ride,
                          opts$summary_plots_selected_colour_swim))
            p <- add_segments(p,
                              data = df_sub,
                              x = ~ start, xend = ~ end,
                              y = ~ sday, yend = ~ eday,
                              color = I(col),
                              size = I(3),
                              hoverinfo = "text",
                              text = ~ sprintf("Session: %s<br>Start: %s <br>End: %s <br>Sport: %s",
                                               df$session[which(df$session %in% session)],
                                               sumX$sessionStart[which(df$session %in% session)],
                                               sumX$sessionEnd[which(df$session %in% session)],
                                               sumX$sport[which(df$session %in% session)]))
        }
        y <- list(title = "")
        x <- list(title = "")
        p <- layout(p, dragmode = "select", showlegend = FALSE, yaxis = y, xaxis = x,
                    plot_bgcolor = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
                    config(displaylogo = FALSE,
                           modeBarButtonsToRemove = list("zoomIn2d",
                                                         "zoomOut2d",
                                                         "autoScale2d",
                                                         "toggleSpikelines",
                                                         "hoverClosestCartesian",
                                             "hoverCompareCartesian"))
        p
    }
    else {
        timeline(sumX)
    }
}
