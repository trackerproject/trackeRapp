## #' Plot training zones.
## #'
## #' @param x An object of class \code{trackeRdata}.
## #' @param session A vector of selected sessions.
## #' @param what A vector of variable names to be plotted.
## #' @param n_zones A numeric. The number of zones to split the dataset into.
## #' @param parallel A logical. Whether use parallel computing.
## #' @param breaks A named list of computed breaks for zones from \code{compute_breaks()}.
plot_zones <- function(x, session, what = c("heart_rate"),
                       n_zones, parallel = TRUE, breaks,
                       options = NULL) {
    opts <- if (is.null(options)) trops() else options
    if (isTRUE(length(session) == 0) | is.null(breaks)) {
        return(plotly_empty(type = "scatter", mode= "markers"))
    }
    if (is.null(what)) {
      return(NULL)
    }
    sports <- get_sport(x)[session]

    x <- zones(x, session = session, what = what, breaks = breaks,
               n_zones = n_zones, parallel = parallel)
    dat <- do.call("rbind", x)
    dat$sports <- rep(sports, each = length(breaks) * n_zones)

    col <- rep(0, length(sports))
    for (sp in c("running", "cycling", "swimming")) {
        cols <- colorRampPalette(opts$zones_colours[[sp]])(sum(sports == sp))
        col[sports == sp] <- cols
    }

    dat$zoneF <- factor(paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"),
                        levels = unique(paste0("[", paste(dat$lower, dat$upper,
                                                          sep = "-"), ")")),
                        ordered = TRUE)
    dat$Session <- paste("Session", sprintf(paste0("%0", nchar(max(dat$session)), "d"),
                                            dat$session))

    dat$timeN <- as.numeric(dat$time)
    ## facets
    units <- getUnits(x)
    individual_plots <- list()
    legend_status <- FALSE
    for (feature in what) {
        y <- list(title = "% of time")
        x <- list(title = lab_data(feature, units))
        feature_zones <- dat[dat$variable == feature, ]
        time_unit <- units(feature_zones$time)
        p <- plot_ly(feature_zones,
                     x = ~ zoneF, y = ~ percent,
                     color = ~ Session, colors = col,#[feature_zones$session],
                     legendgroup = ~ Session, hoverinfo = "text",
                     text = ~ paste0(
                               Session, "\n",
                               round(time, 2), " ", time_unit, " in zone\n",
                               round(percent, 2), "% of session\n")) %>%
            add_bars() %>%
            layout(xaxis = x, yaxis = y, hovermode = "closest",
                   plot_bgcolor = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)")
        individual_plots[[feature]] <- style(p, showlegend = legend_status)
        legend_status <- FALSE
    }

    plots <- subplot(individual_plots,
                     nrows = length(what),
                     margin = 0.05,
                     shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
        config(displayModeBar = FALSE)

    return(plots)
}
