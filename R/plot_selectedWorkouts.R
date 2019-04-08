## #' Plot the profile for each selected session for a given variable (heart rate, altitude, pace).
## #'
## #' @param x An object of class \code{\link{trackeRdata}}.
## #' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
## #' @param sumX An object of class \code{trackeRdataSummary}.
## #' @param what Which variables should be plotted?
## #' @param threshold Logical. Should thresholds be applied?
## #' @param smooth Logical. Should the data be smoothed?
## #' @param trend Logical. Should a smooth trend be plotted?
## #' @param dates Logical. Should the date of the session be used in the panel header?
## #' @param changepoints Logical. Whether changepoints should be identified and plotted.
## #' @param print_changepoints Logical. Whether or not to print changepoint values (when changepoints = TRUE).
## #' @param n_changepoints A numeric. The threshold for the maximum number of changepoints to search for.
## #' @param unit_reference_sport A character. The sport to be used as reference for units.
## #' @param moving_threshold A numeric for the threshold.
## #' @param desampling A numeric proportion between (0-1] for the proportion of raw data to be plotted.
## #' @param y_axis_range A vector with an upper and a lower limit for the given variable.
plot_selected_workouts <- function(x,
                                   session,
                                   what,
                                   sumX,
                                   threshold = TRUE,
                                   smooth = FALSE,
                                   trend = TRUE,
                                   dates = TRUE,
                                   changepoints = FALSE,
                                   n_changepoints = 6,
                                   print_changepoints = FALSE,
                                   unit_reference_sport = NULL,
                                   moving_threshold = NULL,
                                   desampling = 1,
                                   k = 200,
                                   y_axis_range = NULL,
                                   options = NULL) {
    opts <- if (is.null(options)) trops() else options

    if (isTRUE(length(session) == 0)) {
        return(plotly_empty(type = "scatter", mode= "markers"))
    }

    ## altitude a lot less noisy, therefore need to ensure all data shown on graph
    if(what == 'altitude') {
        y_axis_range[[1]] <- y_axis_range[[1]] * 0.80
        y_axis_range[[2]] <- y_axis_range[[2]] * 1.2
    }

    if(what == 'pace') {
        y_axis_range <- y_axis_range[c(2,1)]
    }

    sports <- get_sport(x)[session]

    var_name_units <- lab_sum(feature = what, data = sumX,
                              transform_feature = FALSE)

    var_units <- lab_sum(feature = what, data = sumX,
                         whole_text = FALSE, transform_feature = FALSE)

    x <- x[session]


    ##  Copied from core trackeR
    units <- get_units(x)
    if (is.null(session)) {
        session <- seq_along(x)
    }
    if (is.null(unit_reference_sport)) {
        unit_reference_sport <- find_unit_reference_sport(x)
    }
    ## Match units to those of unit_reference_sport
    un <- collect_units(units, unit_reference_sport)
    for (va in unique(un$variable)) {
        units$unit[units$variable == va] <- un$unit[un$variable == va]
    }
    ## convert moving_threshold
    if (is.null(moving_threshold)) {
        moving_threshold <- c(cycling = 2, running = 1, swimming = 0.5)
        speed_unit <- un$unit[un$variable == "speed"]
        if (speed_unit != "m_per_s") {
            conversion <- match.fun(paste("m_per_s", speed_unit, sep = "2"))
            moving_threshold <- conversion(moving_threshold)
        }
    }

    ## Change units to those of unit_reference_sport
    x <- change_units(x, units$variable, units$unit, units$sport)
    ## threshold
    if (threshold) {
        dots <- list()
        if (all(c("variable", "lower", "upper", "sport") %in% names(dots))) {
            th <- generate_thresholds(dots$variable, dots$lower, dots$upper, dots$sport)
        }
        else {
            th <- generate_thresholds()
            ## IK 01/03/2019: note here the
            ## change_units.trackeRthresholds is not exported from
            ## trackeR so the below will fail
            th <- change_units(th, variable = units$variable, unit = units$unit, sport = units$sport)
        }
        ## apply thresholds
        x <- threshold(x, th$variable, th$lower, th$upper, th$sport)
    }
    #####

    var_name_units <- unique(var_name_units)

    plot_stored <- list()
    images <- list()
    smoothed_values <- list(maximum = numeric(), minimum = numeric())
    n_plot <- 0
    shapes <- list()
    changepoint_y_values <- c()
    step_size <- 1 / length(unique(session))
    start <- 0

    ## Loop through each session
    for (i in seq_along(session)) {
        df_subset <- x[[i]]
        df_subset <- df_subset[, what]
        dates <- index(df_subset)
        df_subset <- data.frame(df_subset)
        names(df_subset) <- what
        df_subset$Index <- dates
        df_subset$id <- csession <- session[i]
        csport <- get_sport(x[i])
        df_subset$SessionID <- paste0(paste(csession, csport, sep = ": "),
                                      "\n", format(df_subset$Index, "%Y-%m-%d"))
        df_subset$numericDate <- as.numeric(df_subset$Index)
        n_plot <- n_plot + 1
        colnames(df_subset)[which(colnames(df_subset) == what)] <- "Value"
        not_na <- !is.na(df_subset[, "Value"])
        non_na_values <- sum(not_na)
        has_values <- non_na_values > 100
        df_subset <- if (has_values) df_subset[not_na, ] else df_subset

        annotations_list <- list(text = paste("Session:", csession),
                                 xref = "paper",
                                 yref = "paper",
                                 yanchor = "bottom",
                                 xanchor = "center",
                                 align = "center",
                                 x = 0.5,
                                 y = 1,
                                 showarrow = FALSE)
        axis_list <- list(zeroline = FALSE, fixedrange = TRUE, tickangle = 0)

        if (has_values) {
            if (changepoints) {
                n_sessions <- length(df_subset$Value) - 5
                m.binseg <- cpt.mean(df_subset$Value[6:n_sessions],
                                     method = "BinSeg",
                                     penalty = "BIC",
                                     minseglen = length(df_subset$Value) / 100, Q = n_changepoints)
                x_values <- c(1, cpts(m.binseg) + 5, length(df_subset$Value))
                y_values <- coef(m.binseg)$mean
                if (print_changepoints) {
                    print(df_subset$Index[cpts(m.binseg)])
                    print(coef(m.binseg))
                }
                ## initiate a line shape object
                line <- list(type = "line",
                             line = list(color = opts$workouts_changepoint_colour, dash = "dot"),
                             xref = paste0("x", n_plot),
                             yref = paste0("y", n_plot))
                line_v <- list(type = "line",
                               line = list(color = opts$workouts_changepoint_colour, dash = "dot"),
                               xref = paste0("x", n_plot),
                               yref = paste0("y", n_plot))
                for (k in c(1:(length(x_values) - 1))) {
                    line[["x0"]] <- df_subset$Index[x_values[k]]
                    line[["x1"]] <- df_subset$Index[x_values[k + 1]]
                    line[c("y0", "y1")] <- y_values[k]
                    changepoint_y_values <- c(changepoint_y_values, y_values[k])
                    shapes[[length(shapes) + 1]] <- line
                }
                for (k in c(2:(length(x_values) - 1))) {
                    line_v[["x0"]] <- df_subset$Index[x_values[k]]
                    line_v[["x1"]] <- df_subset$Index[x_values[k]]
                    line_v[c("y0", "y1")] <- range(df_subset$Value)
                    changepoint_y_values <- c(changepoint_y_values, y_values[k])
                    shapes[[length(shapes) + 1]] <- line_v
                }
            }
            sampled_rows <- sort(sample(index(df_subset),
                                        size = length(index(df_subset)) * desampling))
            col <- ifelse(csport == "running",
                          opts$summary_plots_selected_colour_run,
                   ifelse(csport == "cycling",
                          opts$summary_plots_selected_colour_ride,
                          opts$summary_plots_selected_colour_swim))
            ceg <- what == "cumulative_elevation_gain"
            if (ceg) {
                hovertext <- paste(round(df_subset$Value, 2), var_units)
                a <- plot_ly(df_subset[sampled_rows, ],
                             x = ~ Index, y = ~ Value, hoverinfo = "text",
                             text = hovertext[sampled_rows],
                             type = "scatter", mode = "lines",
                             showlegend = FALSE, alpha = 1, color = I(col))
                a <- add_lines(a, x = ~ Index, y = I(0),
                               type = 'scatter',
                               mode = 'lines',
                               fill = 'tonexty',
                               fillcolor = I(col),
                               hoverinfo = "none",
                               alpha = 0.2,
                               showlegend = FALSE)
            }
            else {
                a <- plot_ly()
            }
            if (smooth & !ceg) {
                ## Using gam
                ## smoothed_model <- gam(Value ~ s(numericDate, bs = "cs"), data = df_subset)
                ## smoothed_data <- predict.gam(smoothed_model, newdata = df_subset)
                ## Using smooth spline
                ## smoothed_model <- smooth.spline(df_subset$numericDate, df_subset$Value)
                ## smoothed_data <- predict(smoothed_model)$y
                ## Using moving averages
                smoothed_data <- rollmedian(zoo(x = df_subset$Value, order.by = df_subset$Index),
                                          k = k, align = "center")

                smoothed_values$minimum <- c(smoothed_values$minimum, min(smoothed_data))
                smoothed_values$maximum <- c(smoothed_values$maximum, max(smoothed_data))
                a <- a %>% add_lines(data = df_subset,
                                     ## x = ~ Index, y = smoothed_data,
                                     x = index(smoothed_data), y = coredata(smoothed_data),
                                     hoverinfo = "text",
                                     text = paste(round(smoothed_data, 2), var_units),
                                     color = I(col),
                                     showlegend = FALSE, alpha = 1)
            }
            a <- a %>% layout(annotations = annotations_list,
                              xaxis = axis_list,
                              yaxis = c(axis_list, list(range = y_axis_range)))
        }
        else {
            maximal_range <- c(-1, 1)
            df_subset$Value <- 0
            a <- plot_ly(df_subset,
                         x = ~ Index, y = ~ Value, hoverinfo = "none",
                         type = "scatter", mode = "none",
                         showlegend = FALSE) %>%
                layout(annotations = annotations_list,
                       xaxis = axis_list,
                       yaxis = c(axis_list, list(range = y_axis_range, showticklabels = TRUE)))
        }
        plot_stored[[as.character(i)]] <- a
        sport_image <- switch(csport,
                              "running" = "running.png",
                              "cycling" = "cycling.png",
                              "swimming" = "swimming.png")

        images[[csession]] <- list(source = sport_image,
                                              xref = "paper",
                                              yref = "paper",
                                              x = start + step_size / 10,
                                              y = 1,
                                              sizex = 0.07,
                                              sizey = 0.07,
                                              opacity = 0.3)
        start <- start + step_size
    }

    y <- list(title = var_units, fixedrange = TRUE)
    x <- list(title = NULL, fixedrange = TRUE)

    return(subplot(plot_stored, nrows = 1,  titleY = FALSE, margin = 0.003) %>%
           config(displayModeBar = FALSE) %>%
           layout(showlegend = FALSE, xaxis = x, yaxis = y, images = images,
                  hovermode = "x", shapes = shapes, dragmode = "pan",
                  plot_bgcolor = "rgba(0, 0, 0, 0)",
                  paper_bgcolor = "rgba(0, 0, 0, 0)"))
}


