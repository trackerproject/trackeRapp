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
                          "avgCadenceRunning" = "cadence_running",
                          "avgHeartRate" = "heart_rate",
                          "avgCadenceCycling" = "cadence_cycling",
                          "wrRatio" = "work-to-rest \n ratio")
    }
    else {
        concept <- feature
    }
    thisunit <- units$unit[units$variable == concept]
    prettyUnit <- prettifyUnits(thisunit)
    if (whole_text) {
        if (transform_feature) {
            ret <- switch(feature,
                          "distance" = paste0("Distance \n[", prettyUnit, "]"),
                          "duration" = paste0("Duration \n[", prettyUnit, "]"),
                          "avgSpeed" = paste0("Average Speed \n[", prettyUnit, "]"),
                          "avgPace" = paste0("Average Pace \n[", prettyUnit, "]"),
                          "avgCadenceRunning" = paste0("Cadence Running \n[", prettyUnit, "]"),
                          "avgCadenceCycling" = paste0("Cadence Cycling \n[", prettyUnit, "]"),
                          "avgPower" = paste0("Average Power \n[", prettyUnit, "]"),
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
                          "speed" = paste0("Speed \n[", prettyUnit, "]"),
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
                          "wrRatio" = "work-to-rest ratio")
        }
        else {
            ret <- switch(feature,
                          "pace" = prettyUnit,
                          "cadence_running" = prettyUnit,
                          "cadence_cycling" = prettyUnit,
                          "heart_rate" = prettyUnit,
                          "altitude" = prettyUnit,
                          "speed" = prettyUnit,
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
      "Work to rest ratio" = "wrRatio")
}

## Generate metrics to test if they have data
workout_view_features <- function() {
    c("Speed" = "speed",
      "Pace" = "pace",
      "Heart Rate" = "heart_rate",
      "Cadence running" = "cadence_running",
      "Cadence cycling" = "cadence_cycling",
      "Power" = "power",
      "Altitude" = "altitude")
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
generate_objects <- function(data, output, session, choices) {
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
    data$identified_sports <- sports_options[identified_sports]
    data$limits <- compute_limits(data$object, a = 0.1)
    data$is_location_data <- sapply(data$object, function(x) {
        size <- nrow(x)
        x_sample <- sample(x[, 'longitude'], round(size / 1000))
        !all((is.na(x_sample)) | (x_sample == 0))
    })
    data$sessions_map <- rep(seq_along(data$object)[data$is_location_data],
                             times = 1, each = 2)
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

## Update selection of sports
## @param data object of class \code{reactivevalues}.
## @param session \code{shiny} object.
update_sport_selection <- function(data, session) {
    updateCheckboxGroupButtons(session = session,
                               inputId = "sports",
                               choices = as.vector(data$identified_sports),
                               selected = as.vector(data$identified_sports),
                               status = "info",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                no = icon("remove", lib = "glyphicon")))
}

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
           "avgHeartRate" = "Average Heart Rate",
           "wrRatio" = "work-to-rest ratio")
}

## Get units of a measurement in a nice format for a given variable.
## @param feature character string; the variable for which to generate units.
## @param units vector of units generated from \code{trackeR} package \code{getUnits()}.
lab_data <- function(feature, units) {
    thisunit <- units$unit[units$variable == feature]
    prettyUnit <- prettifyUnits(thisunit)
    paste0(feature, " [", prettyUnit, "]")
}
