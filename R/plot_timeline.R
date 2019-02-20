#' A timeline plot for workouts.
#'
#' @param sumX An object of class \code{trackeRdataSummary}.
#' @param plotly Logical. Return plotly plots or standard trackeR plots
#' @param shiny Logical. Whether plots are in a shiny environment.
#' @param session A vector. Selected session numbers.
plot_timeline <- function(sumX, session, shiny=TRUE, plotly=TRUE) {
  if (plotly) {
    d <- if (shiny) event_data("plotly_selected") else NULL
    startdates <- as.Date(sumX$sessionStart)
    enddates <- as.Date(sumX$sessionEnd)
    ## Hack to extract times
    endtimes <- as.POSIXct(paste(Sys.Date(), format(sumX$sessionStart, "%H:%M:%S")))
    starttimes <- as.POSIXct(paste(Sys.Date(), format(sumX$sessionEnd, "%H:%M:%S")))
    #
    # endtimes <- as.POSIXct(
    #   as.numeric(difftime(endtimes, trunc(endtimes, "days"), units = "secs")),
    #   origin = Sys.Date()
    # )
    # starttimes <- as.POSIXct(as.numeric(difftime(
    #   starttimes, trunc(starttimes, "days"),
    #   units = "secs"
    # )), origin = Sys.Date())
    df <- data.frame(sday = startdates, eday = enddates, start = starttimes, end = endtimes, session = sumX$session)

    p <- plot_ly()
    p <- add_markers(p, data = df, x = ~ start, y = ~ sday, key = ~ session, alpha = 0, hoverinfo = "none")

    p <- add_segments(
      p,
      data = df, x = ~ start, xend = ~ end, y = ~ sday, yend = ~ eday,
      color = I("deepskyblue3"), hoverinfo = "text",
      text = sprintf(
        "Session: %s<br>Start: %s <br>End: %s",
        df$session, sumX$sessionStart, sumX$sessionEnd
      )
    )

    all_sessions <- nrow(sumX)
    if (!(identical(all_sessions, length(session)))) {
      p <- add_segments(
        p,
        data = df[which(df$session %in% session), ], x = ~ start,
        xend = ~ end, y = ~ sday, yend = ~ eday,
        color = I("darkorange3"), hoverinfo = "text",
        text = ~ sprintf(
          "Session: %s<br>Start: %s <br>End: %s",
          df$session[which(df$session %in% session)],
          sumX$sessionStart[which(df$session %in% session)],
          sumX$sessionEnd[which(df$session %in% session)]
        )
      )
    }
    y <- list(title = "")
    x <- list(title = "")
    p <- layout(p, dragmode = "select", showlegend = FALSE, yaxis = y, xaxis = x) %>%
      config(displayModeBar = F)
    p
  } else {
    timeline(sumX)
  }
}
