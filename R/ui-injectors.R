## Insert map
create_map <- function() {
    insertUI(immediate = TRUE,
             selector = ".content",
             where = "beforeEnd",
             ui = conditionalPanel(
                 condition = "output.cond == true",
                 div(class = "main_plots",
                     fluidRow(
                         box(
                             id = "box1",
                             width = 12,
                             collapsible = TRUE,
                             collapsed = FALSE,
                             title = tagList(icon("map"), "Map"),
                             mapdeck::mapdeckOutput(outputId = "map"))))))
}

## Insert summary boxes
create_summary_boxes <- function() {
    insertUI(immediate = TRUE,
             selector = ".content",
             where = "beforeEnd",
             ui = conditionalPanel(
                 condition = "output.cond == true",
                 div(class = "main_plots",
                     fluidRow(
                         valueBoxOutput("nsessions_running_box", width = 4),
                         valueBoxOutput("nsessions_cycling_box", width = 4),
                         valueBoxOutput("nsessions_swimming_box", width = 4)),
                     fluidRow(
                         valueBoxOutput("avgDistance_box", width = 4),
                         valueBoxOutput("avgDuration_box", width = 4),
                         valueBoxOutput("avgPace_box", width = 4)),
                     fluidRow(
                         valueBoxOutput("avgHeartRate_box", width = 4),
                         valueBoxOutput("avgTemperature_box", width = 4),
                         ## valueBoxOutput("avgAltitude_box", width = 4))
                         valueBoxOutput("total_elevation_gain_box", width = 4))
                     )))
}


## Create workout plots
## @param feature A character. The metric that is plotted, selected from \code{\link{summary_view_features}}.
create_workout_plots <- function(feature) {
    fname <- switch(as.character(feature),
                    "distance" = "Distance",
                    "duration" = "Duration",
                    "avgSpeed" = "Average Speed",
                    "avgPace" = "Average Pace",
                    "avgCadenceRunning" = "Average Cadence Running",
                    "avgCadenceCycling" = "Average Cadence Cycling",
                    "avgPower" = "Average Power",
                    "avgHeartRate" = "Average Heart Rate",
                    "avgTemperature" = "Average Temperature",
                    "avgAltitude" = "Average Altitude",
                    "total_elevation_gain" = "Total elevation gain",
                    "wrRatio" = "Work-to-rest Ratio")
    insertUI(selector = ".content",
             where = "beforeEnd",
             ui = conditionalPanel(
                 condition = paste0("output.", feature, " == false"),
                 div(class = "main_plots", id = paste0("box", feature),
                     fluidRow(
                         box(width = 12,
                             collapsible = TRUE,
                             title = tagList(icon(create_icon(feature)), fname),
                             plotlyOutput(paste0(feature, "_plot"),
                                          width = "auto",
                                          height = "auto"))))))
}

## Create selected_workouts plot
## @param id A character. The ID of the plot.
## @param collapsed A logical. Whether or not the UI box should be collapsed.
create_selected_workout_plot <- function(id, workout_features, collapsed = FALSE) {
    insertUI(
        selector = ".content",
        where = "beforeEnd",
        ui = conditionalPanel(
            condition = paste0("output.", id, " == false"),
            div(class = "plots", id = id,
                fluidRow(
                    box(
                        width = 12,
                        collapsible = TRUE,
                        collapsed = collapsed,
                        title = tagList(icon("gear"),
                                        switch(id,
                                               "pace" = "Pace",
                                               "heart_rate" = "Heart Rate",
                                               "altitude" = "Altitude",
                                               "power" = "Power",
                                               "speed" = "Speed",
                                               "cadence_running" = "Cadence Running",
                                               "cadence_cycling" = "Cadence Cycling",
                                               "altitude" = "Altitude",
                                               "temperature" = "Temperature",
                                               "cumulative_elevation_gain" = "Cumulative elevation gain")),
                        fluidRow(
                        ## column(3,
                        ## dropdownButton(
                        ##     circle = TRUE,
                        ##     up = TRUE,
                        ##     icon = icon("wrench"),
                        ##     size = trops()$dropdown_button_size,
                            ##     ## tooltip = tooltipOptions(title = "Click to see inputs !"),
                            column(3,
                                   selectInput(
                                       inputId = paste0("what2", id),
                                       label = "Shaded feature",
                                       multiple = FALSE,
                                       choices = workout_features, #c("altitude", "temperature", "speed", "pace"),
                                selected = "altitude")),
                            column(1,
                                   selectizeInput(
                                       inputId = paste0("n_changepoints", id),
                                       label = "Changepoints",
                                       multiple = FALSE,
                                       choices = c(
                                           "no" = 0,
                                           "1" = 1,
                                           "2" = 2,
                                           "3" = 3,
                                           "4" = 4,
                                           "5" = 5,
                                           "6" = 6,
                                           "7" = 7,
                                           "8" = 8,
                                           "9" = 9,
                                           "10" = 10,
                                           "11" = 11,
                                           "12" = 12),
                                       selected = "no"))),
                            ## column(1,
                            ## actionButton(paste0("detect_changepoints", id),
                            ##              label = "Detect"))),
                        div(id = "workout_view_plot",
                            uiOutput(paste0(id, "_plot"))))))))
}

## Create concentration profile plot UI.
## @param inputId A character. The ID of the user input for the metrics that should be plotted
## @param plotId A character. The ID of the plot.
## @param choices A vector of the metrics a user can select to be plotted, selected from \code{\link{workout_view_features}}.
## @param collapsed A logical. Whether or not the UI box should be collapsed.
create_profiles_box <- function(inputId, plotId, choices, collapsed = FALSE) {
    insertUI(
        selector = ".content",
        where = "beforeEnd",
        ui = conditionalPanel(
            condition = "output.cond == false",
            div(class = "plots",
                fluidRow(
                    box(
                        width = 12,
                        collapsible = TRUE,
                        collapsed = collapsed,
                        title = tagList(icon("gear"), "Workout concentration"),
                        fluidRow(
                            column(2, pickerInput(inputId = inputId,
                                                  label = "Features",
                                                  choices = choices,
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = TRUE,
                                                  selected = c("speed")))),
                        uiOutput(plotId))))))
}

## Create time in zones plot UI.
## @param inputId A character. The ID of the user input for the metrics that should be plotted.
## @param plotId A character. The ID of the plot.
## @param choices A vector of the metrics a user can select to be plotted, selected from \code{\link{workout_view_features}}.
create_zones_box <- function(inputId, plotId, choices) {
    insertUI(
        selector = ".content",
        where = "beforeEnd",
        ui = conditionalPanel(
            condition = "output.cond == false",
            div(class = "plots",
                fluidRow(
                    box(
                        width = 12,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        title = tagList(icon("gear"), "Time in zones"),
                        fluidRow(
                            column(2, pickerInput(inputId = inputId,
                                                  label ="Features",
                                                  choices = choices,
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = TRUE,
                                                  selected = c("speed"))),
                            column(2, pickerInput(inputId = "n_zones",
                                                  label = "Number of zones:",
                                                  multiple = FALSE,
                                                  choices = c("2" = 2,
                                                              "3" = 3,
                                                              "4" = 4,
                                                              "5" = 5,
                                                              "6" = 6,
                                                              "7" = 7,
                                                              "8" = 8,
                                                              "9" = 9),
                                                  options = list(`actions-box` = TRUE),
                                                  selected = "6"))),
                        uiOutput(plotId))))))
}

## Create a return button from selected workouts plot
## @param sport_options A vector of sports identified from the uploaded sessions.
## @param metrics_available A vector of metrics that are found in the dataset.
create_option_box <- function(sport_options, summary_features_available, workout_features_available) {
    insertUI(
        immediate = TRUE,
        selector = ".content",
        where = "afterBegin",
        ui = div(class = "option_boxes",
                 fluidRow(
                     box(
                         width = 12,
                         collapsible = TRUE,
                         title = "Toolbar",
                         fluidRow(
                             column(3,
                                    actionButton(inputId = "no_sports",
                                                 label = "Clear selection",
                                                 icon = icon("times-circle"))),
                             column(3,
                                    actionButton(inputId = "all_sports",
                                                 label = "Select all",
                                                 icon = icon("times-circle"))),
                             column(2,
                                    actionButton(
                                        inputId = "sport_is_running",
                                        label = "Running",
                                        icon = icon("walking"))),   # TODO: temporary fix, running icon is blank
                             column(2,
                                    actionButton(
                                        inputId = "sport_is_cycling",
                                        label = "Cycling",
                                        icon = icon("bicycle"))),
                             column(2,
                                    actionButton(
                                        inputId = "sport_is_swimming",
                                        label = "Swimming",
                                        icon = icon("swimmer")))),
                         br(),
                         fluidRow(
                             column(3,
                                    conditionalPanel(
                                        condition = "output.cond == false",
                                        actionButton(
                                            inputId = "return_to_main_page",
                                            label = "Summary view",
                                            icon = icon("search-minus"))),
                                    conditionalPanel(
                                        condition = "output.cond == true",
                                        actionButton(inputId = "plotSelectedWorkouts",
                                                     label = "Workout view",
                                                     icon = icon("search-plus")))),
                             column(3, actionButton(inputId = "showModalUnits",
                                                    label = "Change units",
                                                    icon = icon("balance-scale"))),
                             column(6,
                                    conditionalPanel(
                                        condition = "output.cond == true",
                                        pickerInput(inputId = "metricsSelected",
                                                    choices = summary_features_available,
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE, selected = trops()$default_summary_plots)),
                                    conditionalPanel(
                                        condition = "output.cond == false",
                                        pickerInput(inputId = "workout_features_selected",
                                                    choices = workout_features_available,
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE,
                                                    selected = trops()$default_workout_plots))
                                       ))))))
}


## Create a summary and timeline boxes
create_summary_timeline_boxes <- function() {
    insertUI(
        immediate = TRUE,
        selector = ".content",
        where = "beforeEnd",
        ui = div(class = "main_plots",
                 fluidRow(
                     box(
                         id = "summary_box",
                         width = 6,
                         collapsible = TRUE,
                         title = tagList(icon("reorder"), "Workout summary"),
                         DTOutput("summary", height = "365px")),
                     box(
                         id = "workout_timeline_box",
                         width = 6,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         title = tagList(icon("calendar"), "Workout timeline"),
                         plotlyOutput("timeline_plot", height = "365px")))))
}

## Generate a modal window where user can chage units of measurement.
##
## @param data An object of class \code{reactivevalues}.
show_change_unit_window <- function(data) {
    showModal(modalDialog(
        title = "Change units",
        awesomeRadio("altitudeUnits", "Altitude:", c("m" = "m",
                                                     "km" = "km",
                                                     "mi" = "mi",
                                                     "ft" = "ft"),
                     inline = TRUE,
                     selected = get_selected_units("altitude", data)),
        awesomeRadio("distanceUnits", "Distance:", c("m" = "m",
                                                     "km" = "km",
                                                     "mi" = "mi",
                                                     "ft" = "ft"),
            inline = TRUE,
            selected = get_selected_units("distance", data)),
        awesomeRadio("speedUnits", "Speed:", c("m/s" = "m_per_s",
                                               "km/h" = "km_per_h",
                                               "ft/min" = "ft_per_min",
                                               "ft/s" = "ft_per_s",
                                               "mi/h" = "mi_per_h"),
            inline = TRUE,
            selected = get_selected_units("speed", data)),
        awesomeRadio("paceUnits", "Pace:", c("min/km" = "min_per_km",
                                             "min/mi" = "min_per_mi",
                                             "s/min" = "s_per_m"),
            inline = TRUE,
            selected = get_selected_units("pace", data)),
        awesomeRadio("durationUnits", "Duration:", c("seconds" = "s",
                                                     "minutes" = "min",
                                                     "hours" = "h"),
            inline = TRUE,
            selected = get_selected_units("duration", data)),
        awesomeRadio("powerUnits", "Power:", c("W" = "W",
                                               "kW" = "kW"),
            inline = TRUE,
            selected = get_selected_units("power", data)),
        footer = tagList(modalButton("Cancel"),
                         actionButton("updateUnits", "Apply"))))
}

