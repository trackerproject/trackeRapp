plot_selected_workouts2 <- function(x,
                                    session = NULL,
                                    what1,
                                    what2,
                                    ylim1,
                                    ylim2,
                                    changepoints = FALSE,
                                    n_changepoints = 6,
                                    smooth = FALSE,
                                    thin = 1,
                                    k = 101,
                                    na_threshold = 0.5,
                                    options = NULL) {


    opts <- if (is.null(options)) trops() else options

    if (isTRUE(length(session) == 0)) {
        return(plotly_empty(type = "scatter", mode= "markers"))
    }


    if (isTRUE(what1 == "pace")) {
        ylim1 <- ylim1[c(2, 1)]
    }

    if (isTRUE(what2 == "pace")) {
        ylim2 <- ylim2[c(2, 1)]
    }


    if (missing(session)) {
        session <- seq_along(x)
    }

    x <- x[session]
    sports <- get_sport(x)

    units <- get_units(x)
    unit_reference_sport <- find_unit_reference_sport(x)

    var1_units <- get_workout_unit(what = what1, sport = unit_reference_sport, units = units)
    var2_units <- get_workout_unit(what = what2, sport = unit_reference_sport, units = units)

    plots <- list()
    images <- list()

    cumel1 <- what1 == "cumulative_elevation_gain"
    cumel2 <- what2 == "cumulative_elevation_gain"

    n_sessions <- length(session)


    margin <- 0.003

    for (i in seq.int(n_sessions)) {
        shapes <- list()

        current_session <- session[i]
        current_sport <- sports[i]

        var_df <- x[[i]][, c(what1, what2)]
        n_df <- nrow(var_df)

        var1_not_na <- !is.na(var_df[, what1])
        var2_not_na <- !is.na(var_df[, what2])
        var1_has_values <- sum(var1_not_na) > na_threshold * n_df
        var2_has_values <- sum(var2_not_na) > na_threshold * n_df

        ## Annotations
        annotations_list <- list(text = paste("Session:", current_session),
                                 xref = "paper",
                                 yref = "paper",
                                 yanchor = "bottom",
                                 xanchor = "center",
                                 align = "center",
                                 x = 0.5,
                                 y = 1,
                                 showarrow = FALSE)

        ## Axes
        axis_list <- list(zeroline = FALSE, fixedrange = TRUE, tickangle = 0, showticklabels = TRUE)
        y1 <- c(axis_list,
                list(range = ylim1,
                     title = var2_units,
                     side = "left"))
        ## see https://github.com/ropensci/plotly/issues/954 for the reason for overlaying value
        y2 <- c(axis_list,
                list(range = ylim2,
                     title = var2_units,
                     overlaying = paste0("y", ifelse(i == 1, "", 2 * i - 1)),
                     side = "right", showgrid = FALSE))

        x1 <- c(axis_list, list(range = as.list(range(index(var_df))),
                                showspikes = TRUE,
                                spikethickness = 1,
                                spikecolor = "rgb(0,0,0)",
                                spikemode = "across"))

        if (var1_has_values) {
            var_df <- var_df[var1_not_na, ]
            ## Do not smooth cumulative elevation gain
            if (cumel1) c1 <- var_df$value1
            if (cumel2) c2 <- var_df$value2
            if (smooth) {
                var_df <- rollmedian(var_df, k = k, align = "center")
            }
            if (cumel1) var_df$value1 <- c1[index(var_df)]
            if (cumel2) var_df$value2 <- c2[index(var_df)]

            dates <- index(var_df)
            var_df <- as.data.frame(var_df)
            names(var_df) <- c("value1", "value2")
            var_df$date <- dates

            ## colour
            col1 <- ifelse(current_sport == "running",
                           opts$summary_plots_selected_colour_run,
                    ifelse(current_sport == "cycling",
                           opts$summary_plots_selected_colour_ride,
                           opts$summary_plots_selected_colour_swim))


            ## image
            current_sport_image <- switch(current_sport,
                                          "running" = "running.png",
                                          "cycling" = "cycling.png",
                                          "swimming" = "swimming.png")

            ## This will place the image 10% in each plot
            x_image <- (i - 1 + 0.1)/(n_sessions + 1) + margin
            images[[current_session]] <- list(source = current_sport_image,
                                              xref = "paper",
                                              yref = "paper",
                                              x = x_image,
                                              y = 1,
                                              sizex = 0.07,
                                              sizey = 0.07,
                                              opacity = 0.3)


            ## Changepoint detection should applied before thining the data
            if (changepoints) {
                npoints <- nrow(var_df) - 5
                m_binseg <- cpt.mean(var_df$value1[6:npoints],
                                     method = "BinSeg",
                                     penalty = "BIC",
                                     ## minseglen = npoints / 100,
                                     Q = n_changepoints)

                x_values <- c(1, cpts(m_binseg) + 5, npoints)
                y_values <- coef(m_binseg)$mean
                ## initiate a line shape object
                line <- list(type = "line",
                             ## line = list(color = opts$workouts_changepoint_colour, dash = "dash"),
                             line = list(width = 1, dash = "dash"),
                             xref = paste0("x", i),
                             yref = paste0("y", 2 * i - 1))
                line_v <- list(type = "line",
                               ## line = list(color = opts$workouts_changepoint_colour, dash = "dash"),
                               line = list(width = 1, dash = "dash"),
                               xref = paste0("x", i),
                               yref = paste0("y", 2 * i - 1))
                for (pp in c(1:(length(x_values) - 1))) {
                    line[["x0"]] <- var_df$date[x_values[pp]]
                    line[["x1"]] <- var_df$date[x_values[pp + 1]]
                    line[c("y0", "y1")] <- y_values[pp]
                    shapes[[length(shapes) + 1]] <- line
                }
                for (pp in c(2:(length(x_values) - 1))) {
                    line_v[["x0"]] <- var_df$date[x_values[pp]]
                    line_v[["x1"]] <- var_df$date[x_values[pp]]
                    line_v[c("y0", "y1")] <- unlist(ylim1)## range(var_df$value1)
                    shapes[[length(shapes) + 1]] <- line_v
                }

            }

            ## Thining the data
            n_df <- nrow(var_df)
            inds <- round(seq(1, n_df, length.out = round(n_df * thin)))
            var_df <- var_df[inds, ]


            ## Plotting
            v1 <- round(var_df$value1, 2)
            v2 <- round(var_df$value2, 2)
            var_df$text <- paste0(what1, ": ", ifelse(is.na(v1), "", paste(v1, var1_units)), "\n",
                                  what2, ": ", ifelse(is.na(v2), "", paste(v2, var2_units)))


            this_plot <- plot_ly(data = var_df)
            this_plot <- add_lines(this_plot,
                                   x = ~ date, y = ~ value1,
                                   color = I(col1),
                                   ## fill = 'tozeroy',
                                   text = ~ text,
                                   hoverinfo = "text",
                                   yaxis = "y",
                                   showlegend = FALSE)

            if (var2_has_values) {
                this_plot <- add_lines(this_plot,
                                       x = ~ date, y = I(ylim2[[1]]),
                                       color = I(col1),
                                       alpha = 0.1,
                                       hoverinfo = "none",
                                       yaxis = "y2",
                                       showlegend = FALSE)

                this_plot <- add_lines(this_plot,
                                       x = ~ date, y = ~ value2,
                                       color = I(col1),
                                       alpha = 0.3,
                                       line = list(color = I(col1)),
                                       fill = 'tonexty',
                                       hoverinfo = "none",
                                       yaxis = "y2",
                                       showlegend = FALSE)
            }
        }
        else {
            var_df <- var_df[c(1, nrow(var_df)), ]
            dates <- index(var_df)
            var_df <- as.data.frame(var_df)
            names(var_df) <- c("value1", "value2")
            var_df$date <- dates
            var_df$value1 <- unlist(ylim1)
            var_df$value2 <- unlist(ylim2)


            this_plot <- plot_ly(data = var_df)
            this_plot <- add_trace(this_plot,
                                   x = ~ date, y = ~ value1,
                                   hoverinfo = "none",
                                   type = "scatter",
                                   mode = "none",
                                   yaxis = "y",
                                   showlegend = FALSE)

            this_plot <- add_trace(this_plot,
                                   x = ~ date, y = I(ylim2[[1]]),
                                   hoverinfo = "none",
                                   type = "scatter",
                                   mode = "none",
                                   yaxis = "y2",
                                   showlegend = FALSE)

        }

        plots[[as.character(i)]] <- this_plot %>%
            layout(xaxis = x1, yaxis = y1, yaxis2 = y2,
                   annotations = annotations_list,
                   shapes = shapes)

    }

    plots[[as.character(n_sessions + 1)]] <- plotly_empty(type = "scatter", mode= "markers")


    y1 <- list(fixedrange = TRUE)
    y2 <- list(fixedrange = TRUE)

    subplot(plots, nrows = 1, titleY = FALSE, margin = margin) %>%
           config(displayModeBar = FALSE) %>%
        layout(showlegend = FALSE,
               hovermode = "x",
               images = images,
               dragmode = "pan")

}
