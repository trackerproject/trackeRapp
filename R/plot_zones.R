#' Plot training zones.
#'
#' @param x An object of class \code{trackeRdata}.
#' @param session A vector of selected sessions.
#' @param what A vector of variable names to be plotted.
#' @param n_zones A numeric. The number of zones to split the dataset into.
#' @param parallel A logical. Whether use parallel computing.
#' @param breaks A named list of computed breaks for zones from \code{compute_breaks()}.
plot_zones <- function(x, session, what = c("heart_rate"),
                       n_zones, parallel = TRUE, breaks) {

    if (is.null(session)) {
        return(plotly_empty())
    }

  x <- zones(x, session = session, what = what, breaks = breaks,
             n_zones = n_zones, parallel = parallel)

  dat <- do.call("rbind", x)
  dat$zoneF <- factor(
    paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"),
    levels = unique(paste0("[", paste(
      dat$lower, dat$upper,
      sep = "-"
    ), ")")),
    ordered = TRUE
  )
  dat$Session <- paste("Session", sprintf(
    paste0("%0", nchar(max(dat$session)), "d"),
    dat$session
  ))
  dat$timeN <- as.numeric(dat$time)
  ## facets
  units <- getUnits(x)

  pal <- colorRampPalette(c("deepskyblue", "dodgerblue4"))(max(dat$session))

  individual_plots <- list()
  legend_status <- TRUE
  for (feature in what) {
    y <- list(title = "% of time")
    x <- list(title = lab_data(feature, units))
    feature_zones <- dat[dat$variable == feature, ]
    p <- plot_ly(
      feature_zones,
      x = ~ zoneF, y = ~ percent,
      color = ~ Session, colors = pal[feature_zones$session], legendgroup = ~ Session, hoverinfo = "text",
      text = ~ paste0("Proportion of a session: ", round(percent, 1), "%", "\n", Session)
    ) %>%
      add_bars() %>%
      layout(xaxis = x, yaxis = y, hovermode = "closest")
    individual_plots[[feature]] <- style(p, showlegend = legend_status)
    legend_status <- FALSE
  }

  plots <- do.call(subplot, c(
    individual_plots,
    nrows = length(what),
    margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE
  ))

  return(plots)
}
