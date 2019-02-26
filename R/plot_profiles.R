## #' Plot concentration profiles for given variables.
## #'
## #' @param x An object of class \code{trackeRdata}.
## #' @param session A vector of selected sessions.
## #' @param what A vector of variable names to be plotted.
## #' @param profiles_calculated Pre-calculated concentration profiles for all sessions.
plot_concentration_profiles <- function(x, session, profiles_calculated,
                                        what = c("speed")) {

    if (is.null(session)) {
        return(plotly_empty(type = "scatter", mode= "markers"))
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
    pal <- colorRampPalette(trops()$zones_colours)(max(df$Series))
    individual_plots <- list()
    legend_status <- TRUE
    for (feature in what) {
        y <- list(title = "dtime", tickangle = 0)
        x <- list(title = lab_data(feature, units), tickangle = 0)
        var_units <- lab_sum(feature = feature, data = tracker_object,
                             whole_text = FALSE, transform_feature = FALSE)
        feature_profile <- df[df$Profile == feature, ]
        feature_profile$Value[is.na(feature_profile$Value)] <- 0

        p <- plot_ly(feature_profile,
                     x = ~ Index, y = ~ Value,
                     color = ~ series, colors = pal[feature_profile$Series], legendgroup = ~ Series,
                     hoverinfo = "text", text = ~ paste(" Value:", round(Index, 1), var_units, "\n", series)) %>%
            add_lines() %>%
            layout(xaxis = x, yaxis = y, hovermode = "closest")
        individual_plots[[feature]] <- style(p, showlegend = legend_status)
        legend_status <- FALSE
    }

    plots <- subplot(individual_plots,
                     nrows = length(what),
                     margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
        config(displayModeBar = FALSE)
    return(plots)
}
