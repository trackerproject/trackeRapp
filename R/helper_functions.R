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
    concept <- switch(feature, "avgPace" = "pace", "avgSpeed" = "speed",
    "distance" = "distance", "duration" = "duration",
    "avgPower" = "power", "avgCadenceRunning" = "cadence_running",
    "avgHeartRate" = "heart_rate", "avgCadenceCycling" = "cadence_cycling",
    )
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
        "wrRatio" = "work-to-rest \n ratio"
      )
    }
    else {
      ret <- switch(feature,
        "pace" = paste0("Pace \n[", prettyUnit, "]"),
        "cadence_cycling" = paste0("Cadence Cycling \n[", prettyUnit, "]"),
        "cadence_running" = paste0("Cadence Running \n[", prettyUnit, "]"),
        "heart_rate" = paste0("Heart Rate \n[", prettyUnit, "]"),
        "altitude" = paste0("Altitude \n[", prettyUnit, "]"),
        "speed" = paste0("Speed \n[", prettyUnit, "]"),
        "power" = paste0("Power \n[", prettyUnit, "]")
      )
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
        "wrRatio" = "work-to-rest ratio"
      )
    }
    else {
      ret <- switch(feature,
        "pace" = prettyUnit,
        "cadence_running" = prettyUnit,
        "cadence_cycling" = prettyUnit,
        "heart_rate" = prettyUnit,
        "altitude" = prettyUnit,
        "speed" = prettyUnit,
        "power" = prettyUnit
      )
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
    "wrRatio" = "fire"
  )
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

  # unused_variables <- c("latitude", "longitude", "heart.rate", "duration", "temperature")
  # allUnits <- get_units(data$object)$variable[!(get_units(data$object)$variable %in% unused_variables)]
  all_units <- c("altitude", "distance", "speed", "pace")
  units <- c()
  for (i in all_units) {
    units <- c(units, input[[paste0(i, "Units")]])
  }
  if (object == "object") {
    data_updated <- change_units(data[[object]],
      variable = rep(all_units, 3),
      unit = rep(units, 3),
      sport = rep(c("cycling", "running", "swimming"),
        each = 4
      )
    )
    data_updated <- change_units(data_updated,
      variable = "power",
      unit = input$powerUnits,
      sport = "cycling"
    )
  }
  if (object == "summary") {
    data_updated <- change_units(data[[object]],
      variable = all_units,
      unit = units
    )
    data_updated <- change_units(data_updated,
      variable = "power",
      unit = input$powerUnits
    )
    data_updated <- change_units(data_updated, variable = "duration", unit = input[["durationUnits"]])
  }

  return(data_updated)
}

## Generate choices for plots
choices <- function() {
  c(
    "Distance" = "distance",
    "Duration" = "duration",
    "Average speed" = "avgSpeed",
    "Average pace" = "avgPace",
    "Average cadence running" = "avgCadenceRunning",
    "Average cadence cycling" = "avgCadenceCycling",
    "Average power" = "avgPower",
    "Average heart rate" = "avgHeartRate",
    "Work to rest ratio" = "wrRatio"
  )
}

## Generate metrics to test if they have data
metrics <- function() {
  c(
    "Speed" = "speed",
    "Pace" = "pace",
    "Heart Rate" = "heart_rate",
    "Cadence running" = "cadence_running",
    "Cadence cycling" = "cadence_cycling",
    "Power" = "power",
    "Altitude" = "altitude"
  )
}

## Update panel with metrics to plot
## @param session \code{shiny} object.
## @param choices vector of features to plot, see \code{\link{choices}}.
## @param has_data vector with boolean expressions representing which features have data.
update_metrics_to_plot_workouts <- function(session, choices, has_data) {
  updatePickerInput(
    session = session,
    inputId = "metricsSelected",
    choices = c(choices[sapply(choices, function(x) {
      has_data[[x]]
    })]),
    selected = c("distance", "duration", "avgSpeed")
  )
}

## Update metrics to plot for Work capacity and time in zones
## @param id character string; the ID of the input.
## @param session \code{shiny} object.
## @param metrics vector of features to plot, see \code{\link{metrics}}.
## @param has_data vector with boolean expressions representing which features have data.
update_metrics_to_plot_selected_workouts <- function(id, session, metrics, has_data) {
  updateSelectizeInput(
    session = session,
    inputId = id,
    choices = metrics[has_data],
    server = TRUE,
    selected = c("speed")
  )
}

## Download handler
## @param data object of class \code{reactivevalues}.
download_handler <- function(data) {
  downloadHandler(
    filename = function() {
      paste0("trackeRapp_data", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(data$object, file)
    }
  )
}

## Show warning window when no data uploaded
show_warning_window <- function() {
  showModal(modalDialog(
    title = "trackeRapp message",
    div(tags$b(
      "Load processed and/or raw data",
      class = "warningMessage"
    )),
    easyClose = TRUE,
    size = "s"
  ))
}

## Calculate plot height for either time in zones or work capacity
## @param metrics vector of metrics that will be plotted.
calculate_plot_height <- function(metrics) {
  paste0(250 * length(metrics), "px")
}

## Get Javascript code for reseting page and collapsing boxes
get_javascript <- function() {
  "
    shinyjs.collapse = function(boxid) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
    };
    // shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }
    shinyjs.reset_page = function() { location.reload(); };
    shinyjs.resetSelection = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); };
    shinyjs.initialize_map_collapse = function() {
      Shiny.onInputChange('is_collapse_box1', 'none')
    };
    shinyjs.check_map_collapse = function() {
      $('#box1').css('display')
    };
    shinyjs.is_map_collapse = function() {

        $('#box1').parent().find('button').click(function(){
          // alert($('#box1').css('display'));
          Shiny.onInputChange('is_collapse_box1', $('#box1').css('display'))
        })
    };
  "
 }
# $('#box1').closest('.box').on('hidden.bs.collapse', function () {});
# $('#box1').closest('.box').on('shown.bs.collapse', function () {})


## Process \code{trackeRdata} object by setting thresholds to remove wrong values, change units, set a moving threshold and test which variables contain data
## @param data object of class \code{reactivevalues}.
process_dataset <- function(data) {
  data$object <- change_units(data$object,
    variable = rep(c(
      "distance", "pace",
      "speed"
    ), 3),
    unit = rep(c("km", "min_per_km", "km_per_h"), 3),
    sport = rep(c("cycling", "running", "swimming"),
      each = 3
    )
  )

  # data$object <- threshold(data$object)
  # data$object <- threshold(data$object,
  #                          variable = rep("distance", 3),
  #                          lower = rep(0, 3),
  #                          upper = rep(500000, 3),
  #                          sport = c("cycling", "running", "swimming")
  #                          )

  # Create trackeRdataSummary object
  data$summary <- summary(data$object, movingThreshold = 0.4)
  data$summary <- change_units(data$summary,
    variable = "duration",
    unit = "h"
  )
  # Test if data in each element of trackeRdataSummary object
  data$hasData <- lapply(data$summary, function(session_summaries) {
    !all(is.na(session_summaries) | session_summaries == 0)
  })
}


# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[["attribs"]][["id"]]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        # img(src = "www/ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(
        class = "btn-err",
        div(
          icon("exclamation-circle"),
          tags$b("Error: "),
          span(class = "btn-err-msg")
        )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  disable(buttonId)
  show(selector = loadingEl)
  hide(selector = doneEl)
  hide(selector = errEl)
  on.exit({
    enable(buttonId)
    hide(selector = loadingEl)
  })

  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    show(selector = doneEl)
    delay(2000, hide(
      selector = doneEl, anim = TRUE, animType = "fade",
      time = 0.5
    ))
    value
  }, error = function(err) {
    errorFunc(err, buttonId)
  })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  html(html = errMessage, selector = errElMsg)
  show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
.warningMessage {
   font-size: 20px;
}
hr {
border-top: 1px solid;
}
a#download_data {
color: #333;
}


.main_plots rect.legendtoggle {
display: none;
}

.main_plots rect.legendtoggle {
cursor: default;
}
"

# Update map based on current selection
update_map <- function(session, data, longitude, latitude) {
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(line.color = "rgba(238, 118, 0, 1)"), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(line.fillcolor = "rgba(238, 118, 0, 1)"), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(line.width = 4), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(opacity = 0.8), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(hovertextsrc = 'ble'), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(line.color = "rgba(0, 154, 205, 1)"), as.list(which(!(data$sessions_map %in% data$selectedSessions)) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(line.fillcolor = "rgba(0, 154, 205, 1)"), as.list(which(!(data$sessions_map %in% data$selectedSessions)) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "restyle",
    list(opacity = 0.2), as.list(which(!(data$sessions_map %in% data$selectedSessions)) - 1)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "relayout",
    list(mapbox.zoom = 9)
  )
  plotlyProxy("map", session) %>% plotlyProxyInvoke(
    "relayout",
    list(mapbox.center = list(
      lat = median(latitude),
      lon = median(longitude)
    ))
  )
}

## ## Calculate plot height for work capacity
## calculate_plot_height_work_capacity <- function(name, data) {
##   if (name != 'work_capacity') {
##     "250px"
##   } else {
##     sports <- unique(sport(data$object[data$selectedSessions]))
##     # Work capacity only for running and cycling
##     sports <- intersect(c('running', 'cycling'), sports)
##     paste0(250 * length(sports), "px")
##   }
## }

## Plot work capacities for each sport
## @param x object of class \code{\link{trackeRdata}}.
## @param session numeric vector of the sessions to be used, defaults to all sessions.
## @param cp numeric. Critical power/speed, i.e., the power/speed which can be maintained for longer period of time.
plot_work_capacities <- function(x, session, cp) {
  sports <- unique(get_sport(x[session]))
  # Work capacity only for running and cycling
  sports <- intersect(c("running", "cycling"), sports)
  if (length(sports) == 1) {
    return(plot_work_capacity(x = x, session = session, cp = cp))
  } else {
    cycling_sessions <- session[get_sport(x[session]) == "cycling"]
    plot_cycling <- plot_work_capacity(
      x = x, session = c(cycling_sessions, -1),
      cp = cp
    )
    running_sessions <- session[get_sport(x[session]) == "running"]
    plot_running <- plot_work_capacity(
      x = x, session = running_sessions,
      cp = cp
    )
    plots <- do.call(subplot, c(
      list(plot_cycling, plot_running),
      nrows = 2,
      margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE
    ))
    return(plots)
  }
}

## Show a modal window to inform a user that no data was selected
show_warning_no_data_selected <- function() {
  showModal(modalDialog(
    title = "trackeRapp message",
    div(tags$b(
      "Choose a processed file and/or at least one raw data file",
      class = "warningMessage"
    )),
    size = "s",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("uploadSampleDataset", "Upload sample dataset")
    )
  ))
}

## Classify sessions by sport, process dataset, generate download handler,
## generate selected sessions object, update what metrics are available
## to plot and other minor actions.
## @param data object of class \code{reactivevalues}.
## @param output \code{shiny} object.
## @param session \code{shiny} object.
## @param choices vector. See \code{\link{choices}}.
generate_objects <- function(data, output, session, choices) {
  process_dataset(data)
  output$download_data <- download_handler(data)
  disable(selector = "#processedDataPath")
  data$selectedSessions <- data$summary$session

  click("createDashboard")
  # TODO incorporate update
  # update_metrics_to_plot_workouts(session, choices, data$hasData)
  sports_options <- c("Running" = "running",
                      "Cycling" = "cycling",
                      "Swimming" = "swimming")
  identified_sports <- sports_options %in% unique(get_sport(data$object))
  data$identified_sports <- sports_options[identified_sports]
  data$limits <- compute_limits(data$object, a = 0.1)
  data$is_location_data <- sapply(data$object,
                                  function(x) {
                                    size <- nrow(x)
                                    x_sample <- sample(x[, 'longitude'], round(size / 1000))
                                    !all((is.na(x_sample)) | (x_sample == 0))
                                    }
  )
  data$sessions_map <- rep(seq_along(data$object)[data$is_location_data],
                           times = 1, each = 2)
}

## Test whether we can plot work capacity for at least one of cycling or running.
## @param data An object of class \code{reactivevalues}.
test_work_capacity <- function(data) {
  selected_sports <- unique(get_sport(data$object[data$selectedSessions]))
  is_data_power <- !all(sapply(data$object[data$selectedSessions], function(x) {
    all((is.na(x[, "power"])) | (x[, "power"] == 0))
  }))

  # Test for power if cycling
  if (("cycling" %in% selected_sports) & (is_data_power)) {
    cycling <- "cycling"
  } else {
    cycling <- NULL
  }
  # Test if running selected
  if ("running" %in% selected_sports) {
    running <- "running"
  } else {
    running <- NULL
  }
  return(c(cycling, running))
}

## Update selection of sports
## @param data object of class \code{reactivevalues}.
## @param session \code{shiny} object.
update_sport_selection <- function(data, session) {
  updateCheckboxGroupButtons(
    session = session,
    inputId = "sports",
    choices = as.vector(data$identified_sports),
    selected =as.vector(data$identified_sports),
    status = "info",
    checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                     no = icon("remove", lib = "glyphicon"))
  )
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
    "wrRatio" = "work-to-rest ratio"
  )
}

## Get units of a measurement in a nice format for a given variable.
## @param feature character string; the variable for which to generate units.
## @param units vector of units generated from \code{trackeR} package \code{getUnits()}.
lab_data <- function(feature, units) {
  thisunit <- units$unit[units$variable == feature]
  prettyUnit <- prettifyUnits(thisunit)
  paste0(feature, " [", prettyUnit, "]")
}
