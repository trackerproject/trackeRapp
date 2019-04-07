get_workout_unit <- function(what, sport, units) {
    if (what == "cumulative_elevation_gain") {
        what <- "altitude"
    }
    thisunit <- units$unit[units$variable == what & units$sport == sport]
    prettifyUnits(thisunit)
}

## Generate a character of formatted units, either only the unit (e.g "[bpm]") or whole text (e.g. "Heart Rate [bpm]").
##
## @param feature character string representing the feature whose units we want to generate.
## @param data object of class \code{trackeRdataSummary} or \code{trackeRdata}.
## @param whole_text generate only unit (e.g "[bpm]") or whole text (e.g. "Heart Rate [bpm]").
## @param transform_feature if \code{TRUE}, expected format of \code{feature} is such as "avgCadence", "avgPower". If \code{FALSE}, expected format is "pace", "cadence", "heart_rate" or "altitude".
lab_sum <- function(feature, data, whole_text = TRUE, transform_feature = TRUE) {
    feature <- as.character(feature)
    units <- getUnits(data)
    if (transform_feature) {
        concept <- switch(feature,
                          "avgPace" = "pace",
                          "avgSpeed" = "speed",
                          "distance" = "distance",
                          "duration" = "duration",
                          "avgPower" = "power",
                          "avgAltitude" = "altitude",
                          "avgTemperature" = "temperature",
                          "avgCadenceRunning" = "cadence_running",
                          "avgHeartRate" = "heart_rate",
                          "avgCadenceCycling" = "cadence_cycling",
                          "total_elevation_gain" = "total_elevation_gain",
                          "wrRatio" = "work-to-rest \n ratio")
    }
    else {
        concept <- feature
    }
    if (feature == "total_elevation_gain" | feature == "cumulative_elevation_gain") {
        thisunit <- units$unit[units$variable == "altitude"]
    }
    else {
        thisunit <- units$unit[units$variable == concept]
    }
    prettyUnit <- prettifyUnits(thisunit)
    if (whole_text) {
        if (transform_feature) {
            ret <- switch(feature,
                          "distance" = paste0("Distance \n[", prettyUnit, "]"),
                          "duration" = paste0("Duration \n[", prettyUnit, "]"),
                          "avgSpeed" = paste0("Average Speed \n[", prettyUnit, "]"),
                          "avgPace" = paste0("Average Pace \n[", prettyUnit, "]"),
                          "avgAltitude" = paste0("Average Altitude \n[", prettyUnit, "]"),
                          "avgTemperature" = paste0("Average Temperature \n[", prettyUnit, "]"),
                          "avgCadenceRunning" = paste0("Cadence Running \n[", prettyUnit, "]"),
                          "avgCadenceCycling" = paste0("Cadence Cycling \n[", prettyUnit, "]"),
                          "avgPower" = paste0("Average Power \n[", prettyUnit, "]"),
                          "total_elevation_gain" = paste0("Total elevation gain \n[", prettyUnit, "]"),
                          "avgHeartRate" = paste0("Average Heart Rate \n[", prettyUnit, "]"),
                          "wrRatio" = "work-to-rest \n ratio")
        }
        else {
            ret <- switch(feature,
                          "pace" = paste0("Pace \n[", prettyUnit, "]"),
                          "cadence_cycling" = paste0("Cadence Cycling \n[", prettyUnit, "]"),
                          "cadence_running" = paste0("Cadence Running \n[", prettyUnit, "]"),
                          "heart_rate" = paste0("Heart Rate \n[", prettyUnit, "]"),
                          "altitude" = paste0("Altitude \n[", prettyUnit, "]"),
                          "temperature" = paste0("Temperature \n[", prettyUnit, "]"),
                          "speed" = paste0("Speed \n[", prettyUnit, "]"),
                          "total_elevation_gain" = paste0("Total elevation gain \n[", prettyUnit, "]"),
                          "power" = paste0("Power \n[", prettyUnit, "]"))
        }
    }
    else {
        if (transform_feature) {
            ret <- switch(feature,
                          "distance" = prettyUnit,
                          "duration" = prettyUnit,
                          "avgSpeed" = prettyUnit,
                          "avgPace" = prettyUnit,
                          "avgCadenceRunning" = prettyUnit,
                          "avgCadenceCycling" = prettyUnit,
                          "avgPower" = prettyUnit,
                          "avgHeartRate" = prettyUnit,
                          "avgAltitude" = prettyUnit,
                          "avgTemperature" = prettyUnit,
                          "total_elevation_gain" = prettyUnit,
                          "wrRatio" = "")
        }
        else {
            ret <- switch(feature,
                          "pace" = prettyUnit,
                          "cadence_running" = prettyUnit,
                          "cadence_cycling" = prettyUnit,
                          "heart_rate" = prettyUnit,
                          "altitude" = prettyUnit,
                          "temperature" = prettyUnit,
                          "speed" = prettyUnit,
                          "cumulative_elevation_gain" = prettyUnit,
                          "power" = prettyUnit)
        }
    }
    unique(ret)
}

## Generate an icon for a given feature.
##
## @param feature character string representing the feature for which we want to generate an icon
create_icon <- function(feature) {
    icon <- switch(feature,
                   "distance" = "area-chart",
                   "duration" = "clock-o",
                   "avgSpeed" = "line-chart",
                   "avgAltitude" = "arrows-v",
                   "total_elevation_gain" = "arrows-v",
                   "avgTemperature" = "thermometer",
                   "nsessions_cycling" = "bicycle",
                   "nsessions_running" = "walking",
                   "nsessions_swimming" = "swimmer",
                   "avgPace" = "tachometer",
                   "avgCadenceRunning" = "tachometer",
                   "avgCadenceCycling" = "tachometer",
                   "avgPower" = "flash",
                   "avgHeartRate" = "heartbeat",
                   "wrRatio" = "fire")
    icon
}

## Get units of measurement for a given feature
##
## @param feature character string for the feature whose units we want to access; for example \code{"altitude"}, \code{"distance"}.
## @param data object of class \code{reactivevalues}
get_selected_units <- function(feature, data) {
    getUnits(data$summary)$unit[getUnits(data$summary)$variable %in% feature][1]
}


## Change units of variables.
##
## @param data object of class \code{reactivevalues}.
## @param input object of class \code{reactivevalues}.
## @param object \code{"summary"} or \code{"object"} to specify which objects' units to change.
change_object_units <- function(data, input, object) {
    all_units <- c("altitude", "distance", "speed", "pace")
    units <- c()
    for (i in all_units) {
        units <- c(units, input[[paste0(i, "Units")]])
    }
    if (object == "object") {
        data_updated <- change_units(data[[object]],
                                     variable = rep(all_units, 3),
                                     unit = rep(units, 3),
                                     sport = rep(c("cycling", "running", "swimming"), each = 4))
        data_updated <- change_units(data_updated,
                                     variable = "power",
                                     unit = input$powerUnits,
                                     sport = "cycling")
    }
    if (object == "summary") {
        data_updated <- change_units(data[[object]],
                                     variable = all_units,
                                     unit = units)
        data_updated <- change_units(data_updated,
                                     variable = "power",
                                     unit = input$powerUnits)
        data_updated <- change_units(data_updated, variable = "duration", unit = input[["durationUnits"]])
    }
    return(data_updated)
}

## Generate summary names for plots
summary_view_features <- function() {
    c("Distance" = "distance",
      "Duration" = "duration",
      "Average speed" = "avgSpeed",
      "Average pace" = "avgPace",
      "Average cadence running" = "avgCadenceRunning",
      "Average cadence cycling" = "avgCadenceCycling",
      "Average power" = "avgPower",
      "Average heart rate" = "avgHeartRate",
      "Work to rest ratio" = "wrRatio",
      "Average altitude" = "avgAltitude",
      "Total elevation gain" = "total_elevation_gain",
      "Average temperature" = "avgTemperature")
}

## Generate metrics to test if they have data
workout_view_features <- function() {
    c("Speed" = "speed",
      "Pace" = "pace",
      "Heart Rate" = "heart_rate",
      "Cadence running" = "cadence_running",
      "Cadence cycling" = "cadence_cycling",
      "Power" = "power",
      "Altitude" = "altitude",
      "Cumulative elevation gain" = "cumulative_elevation_gain",
      "Temperature" = "temperature")
}

## Process \code{trackeRdata} object by setting thresholds to remove wrong values, change units, set a moving threshold and test which variables contain data
## @param data object of class \code{reactivevalues}.
process_dataset <- function(data) {
    data$object <- change_units(data$object,
                                variable = rep(c("distance", "pace", "speed"), 3),
                                unit = rep(c("km", "min_per_km", "km_per_h"), 3),
                                sport = rep(c("cycling", "running", "swimming"), each = 3))

    ## Create trackeRdataSummary object
    data$summary <- summary(data$object, movingThreshold = 0.4)
    data$summary <- change_units(data$summary, variable = "duration", unit = "h")


    ## Test if there is data in each feature in a trackeRdataSummary object
    data$has_data <- lapply(data$summary, function(session_summaries) {
        !all(is.na(session_summaries) | session_summaries == 0)
    })

}

## Update map based on current selection
update_map <- function(session, data, longitude, latitude) {
    opts <- trops()
    to_be_coloured <- data$sessions_map %in% data$selected_sessions
    ## Blues
    plotlyProxy("map", session) %>%
        plotlyProxyInvoke("restyle",
                          list(line.color = opts$summary_plots_deselected_colour,
                               line.fillcolor = opts$summary_plots_deselected_colour))
    ## Oranges
    if (!all(!to_be_coloured)) {
        plotlyProxy("map", session) %>%
            plotlyProxyInvoke("restyle",
                              list(line.color = opts$summary_plots_selected_colour,
                                   line.fillcolor = opts$summary_plots_selected_colour),
                              as.list(which(to_be_coloured) - 1))
        plotlyProxy("map", session) %>%
            plotlyProxyInvoke("restyle",
                              list(hovertextsrc = 'ble'),
                              as.list(which(to_be_coloured) - 1))
    }
    plotlyProxy("map", session) %>%
        plotlyProxyInvoke("relayout", list(mapbox.zoom = 9))
    plotlyProxy("map", session) %>%
        plotlyProxyInvoke("relayout",
                          list(mapbox.center = list(lat = median(latitude), lon = median(longitude))))
}


## Classify sessions by sport, process dataset, generate download handler,
## generate selected sessions object, update what metrics are available
## to plot and other minor actions.
## @param data object of class \code{reactivevalues}.
## @param output \code{shiny} object.
## @param session \code{shiny} object.
## @param choices vector. See \code{\link{summary_view_features}}.
generate_objects <- function(data, output, session, choices, options = NULL) {
    opts <- if (is.null(options)) trops() else options
    process_dataset(data)
    output$download_data <- downloadHandler(filename = paste0("trackeRapp_data", Sys.Date(), ".rds"),
                                            content = function(file) saveRDS(data$object, file))
    disable(selector = "#processedDataPath")
    data$selected_sessions <- data$summary$session
    click("createDashboard")
    sports_options <- c("Running" = "running",
                        "Cycling" = "cycling",
                        "Swimming" = "swimming")
    identified_sports <- sports_options %in% unique(get_sport(data$object))
    data$sports <- sports_options[identified_sports]
    data$identified_sports <- sports_options[identified_sports]
    data$limits0 <- trackeR::compute_limits(data$object, a = opts$quantile_for_limits)
    data$has_metrics <- do.call("rbind", lapply(data$object, function(x) colMeans(is.na(x)) < 1))
    data$is_location_data <- sapply(data$object, function(x) {
        size <- nrow(x)
        x_sample <- sample(x[, 'longitude'], round(size / 1000))
        !all((is.na(x_sample)) | (x_sample == 0))
    })
    data$dummy <- data$dummy + 1
}

## Test whether we can plot work capacity for at least one of cycling or running.
## @param data An object of class \code{reactivevalues}.
test_work_capacity <- function(data) {
    selected_sports <- unique(get_sport(data$object[data$selected_sessions]))
    is_data_power <- !all(sapply(data$object[data$selected_sessions], function(x) {
        all((is.na(x[, "power"])) | (x[, "power"] == 0))
    }))

    ## Test for power if cycling
    if (("cycling" %in% selected_sports) & (is_data_power)) {
        cycling <- "cycling"
    }
    else {
        cycling <- NULL
    }
    ## Test if running selected
    if ("running" %in% selected_sports) {
        running <- "running"
    }
    else {
        running <- NULL
    }
    return(c(cycling, running))
}

## ## Update selection of sports
## ## @param data object of class \code{reactivevalues}.
## ## @param session \code{shiny} object.
## update_sport_selection <- function(data, session) {
##     updateCheckboxGroupButtons(session = session,
##                                inputId = "sports",
##                                choices = as.vector(data$identified_sports),
##                                selected = as.vector(data$identified_sports),
##                                checkIcon = list(yes = icon("ok", lib = "glyphicon"),
##                                                 no = icon("remove", lib = "glyphicon")))
## }

## Convert name
## @param what A character. The metric to convert.
convert_to_name <- function(what) {
    switch(what,
           "distance" = "Distance",
           "duration" = "Duration",
           "avgSpeed" = "Average Speed",
           "avgPace" = "Average Pace",
           "avgCadenceRunning" = "Cadence Running",
           "avgCadenceCycling" = "Cadence Cycling",
           "avgPower" = "Average Power",
           "avgAltitude" = "Average Altitude",
           "avgHeartRate" = "Average Heart Rate",
           "avgTemperature" = "Average Temperature",
           "wrRatio" = "work-to-rest ratio")
}

## Get units of a measurement in a nice format for a given variable.
## @param feature character string; the variable for which to generate units.
## @param units vector of units generated from \code{trackeR} package \code{getUnits()}.
lab_data <- function(feature, units, onlyunit = FALSE) {
    thisunit <- units$unit[units$variable == feature]
    prettyUnit <- prettifyUnits(thisunit)
    if (onlyunit) {
        paste0("[", prettyUnit, "]")
    }
    else {
        paste0(feature, " [", prettyUnit, "]")
    }
}


## Render data in summary box
## @param short_name A character. The metric name, e.g., distance.
## @param long_name A character. The title of the box.
## @param data An object of class \code{reactivevalues}.
render_summary_box <- function(short_name, long_name, data) {
    opts <- trops()
    box_text <- function(what, subtitle, icon, data) {
        value <- reactive({
            if (what == "nsessions_running") {
                sports <- data$summary[data$selected_sessions][["sport"]]
                value <- sum(sports == "running")
            }
            if (what == "nsessions_cycling") {
                sports <- data$summary[data$selected_sessions][["sport"]]
                value <- sum(sports == "cycling")
            }
            if (what == "nsessions_swimming") {
                sports <- data$summary[data$selected_sessions][["sport"]]
                value <- sum(sports == "swimming")
            }
            if (what == "total_elevation_gain") {
                value <- data$summary[data$selected_sessions][[what]]
                value <- round(sum(value[is.finite(value)], na.rm = TRUE), 1)
            }

            if (!(what %in% c("nsessions_swimming", "nsessions_running", "nsessions_cycling",
                              "total_elevation_gain"))) {
                value <- data$summary[data$selected_sessions][[what]]
                value <- round(mean(value[is.finite(value)], na.rm = TRUE), 1)
            }
            if (is.na(value)) {
                "not available"
            }
            else {
                paste0(value, " ", unique(lab_sum(what, data$summary, FALSE)))
            }
        })
        color <- switch(what,
                        "nsessions_cycling" = opts$summary_box_ride_colour,
                        "nsessions_swimming" = opts$summary_box_swim_colour,
                        "nsessions_running" = opts$summary_box_run_colour,
                        opts$summary_box_ok_colour)
        color <- ifelse(value() == "not available", opts$summary_box_na_colour, color)
        valueBox(value(), subtitle, icon, color = color)
    }
    renderValueBox({
        box_text(what = short_name,
                 subtitle = long_name,
                 icon = icon(create_icon(short_name)),
                 data = data)
    })
}

## Generate an object with selected sessions
## @param data An object of class \code{reactivevalues}.
## @param input A shiny object with user input.
## @param plot_selection A logical. Whether session selection made from a plot.
## @param sport_selection A logical. Whether session selection made from sport selector.
## @param table_selection A logical. Whether session selection made from the summary table.
## @param no_selection A logical. Whether no sessions are selected.
generate_selected_sessions_object <- function(data, input,
                                              plot_selection = FALSE,
                                              sport_selection = FALSE,
                                              table_selection = FALSE,
                                              no_selection = FALSE) {
    data$hover <- event_data("plotly_selected")
    if (sport_selection) {
        data$selected_sessions <- data$summary$session[get_sport(data$object) %in% data$sports]
    }
    if (plot_selection) {
        sessions <- data$hover$key
        sessions[sapply(sessions, is.null)] <- NULL
        data$selected_sessions <- unique(na.omit(as.numeric(sessions)))
    }
    if (table_selection) {
        data$selected_sessions <- input$summary_rows_selected
    }
    if (no_selection) {
        data$selected_sessions <- NULL #data$summary$session
    }
    data$selected_sessions <- sort(data$selected_sessions)
}

## Render summary table
## @param data An object of class \code{reactivevalues}.
## @param input A shiny object with user input.
render_summary_table <- function(data, input, options = NULL) {
    opts <- if (is.null(options)) trops() else options
    renderDT({
        dataSelected <- data.frame("Session" = data$summary[["session"]],
                                   "Day" = format(data$summary[["sessionStart"]],
                                                  format = "%a"),
                                   "Date" = format(data$summary[["sessionStart"]],
                                                   format = "%d %b %Y"),
                                   "Start" = format(data$summary[["sessionStart"]],
                                                    format = "%H:%M"),
                                   "End" = format(data$summary[["sessionEnd"]],
                                                  format = "%H:%M"),
                                   "Duration" = paste(round(data$summary[["duration"]], 1),
                                                      lab_sum("duration", data = data$summary,
                                                              whole_text = FALSE)),
                                   "Sport" = get_sport(data$object))
        out <- datatable(dataSelected,
                         rownames = FALSE,
                         autoHideNavigation = TRUE,
                         options = list(paging = FALSE, scrollY = "295px", info = FALSE,
                                        drawCallback = JS(opts$dt_callback_js)))
        formatStyle(out,
                    c("Session", "Day", "Date", "Start", "End", "Duration", "Sport"),
                    backgroundColor = opts$summary_plots_deselected_colour)
    })
}

## Convert trackeRdata and trackeRdataSummary to an sf object  for mapdeck
get_coords <- function(data, sessions = NULL, keep = 0.1) {
    popupText <- function(session) {
        w <- which(sumX$session == session)
        paste(
            paste("Session:", session),
            paste(sumX$sport[w]),
            paste(sumX$sessionStart[w], "-", sumX$sessionEnd[w]),
            paste("Distance:", round(sumX$distance[w], 2), un$unit[un$variable == "distance"]),
            paste("Duration:", round(as.numeric(sumX$duration[w]), 2), units(sumX$duration[w])),
            sep = "<br/>"
        )
    }
    x <- data$object
    sumX <- data$summary
    units <- get_units(x)
    unit_reference_sport <- find_unit_reference_sport(x)
    un <- collect_units(units, unit_reference_sport)

    if (is.null(sessions)) {
        sessions <- seq_along(x)
    }
    geometry <- lapply(sessions, function(s) {
        coord <- coredata(x[[s]])[, c("longitude", "latitude")]
        subsample <- seq(1, nrow(coord), length.out = ceiling(keep * nrow(coord)))
        st_multilinestring(list(coord[subsample, ]))
    })
    tooltips <- sapply(sessions, popupText)
    st_sf(session = sessions,
          sport = sumX$sport[sessions],
          tooltip = tooltips,
          geometry = geometry,
          crs = 4326)
}
