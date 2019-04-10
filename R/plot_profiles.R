## #' Plot concentration profiles for given variables.
## #'
## #' @param x An object of class \code{trackeRdata}.
## #' @param session A vector of selected sessions.
## #' @param what A vector of variable names to be plotted.
## #' @param profiles_calculated Pre-calculated concentration profiles for all sessions.
plot_concentration_profiles <- function(x,
                                        session,
                                        profiles_calculated,
                                        what = c("speed"),
                                        limits = NULL,
                                        options = NULL) {

    opts <- if (is.null(options)) trops() else options

    if (isTRUE(length(session) == 0) | is.null(profiles_calculated)) {
        return(plotly_empty(type = "scatter", mode= "markers"))
    }

    if (is.null(what)) {
      return(NULL)
    }

    sports <- get_sport(x)[session]
    col <- rep(0, length(sports))
    for (sp in c("running", "cycling", "swimming")) {
        cols <- colorRampPalette(opts$zones_colours[[sp]])(sum(sports == sp))
        col[sports == sp] <- cols
    }

    tracker_object <- x
    ## Generate concentration profile
    x <- get_profile(object = profiles_calculated, session = session, what = what)
    ## duration unit; sport does not matter here as units have been uniformised already
    units <- get_units(x)
    duration_unit <- units$unit[units$sport == "running" & units$variable == "duration"]
    ## fortify
    df <- fortify_conProfile(x, melt = TRUE)
    df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "session"), function(x) x[2]))
    df$Profile <- factor(df$Profile)
    df$series <- paste("Session", sprintf(paste0("%0", nchar(max(df$Series)), "d"), df$Series))
    ## pal <- colorRampPalette(opts$zones_colours)(max(df$Series))
    individual_plots <- list()
    legend_status <- FALSE

    for (feature in what) {
        y <- list(title = "dtime", tickangle = 0)
        x <- list(title = lab_data(feature, units),
                  range = limits[[feature]],
                  tickangle = 0)
        var_units <- lab_sum(feature = feature, data = tracker_object,
                             whole_text = FALSE, transform_feature = FALSE)
        feature_profile <- df[df$Profile == feature, ]

        if (feature != "heart_rate") {
            feature_profile$Value[is.na(feature_profile$Value)] <- 0
        }

        p <- plot_ly(feature_profile,
                     x = ~ Index, y = ~ Value,
                     color = ~ series, colors = col, #pal[feature_profile$Series],
                     legendgroup = ~ Series,
                     hoverinfo = "text", text = ~ paste(" Value:", round(Index, 2), var_units, "\n", series)) %>%
            add_lines() %>%
            layout(xaxis = x, yaxis = y, hovermode = "closest",
                   plot_bgcolor = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)")
        individual_plots[[feature]] <- style(p, showlegend = legend_status)
        legend_status <- FALSE
    }

    plots <- subplot(individual_plots,
                     nrows = length(what),
                     margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
        config(displayModeBar = FALSE)
    return(plots)
}
