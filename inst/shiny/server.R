## Live version configuration
## set to TRUE only for a live version
live_version <- FALSE
if (isTRUE(live_version)) {
    library("trackeR")
    library("shiny")
    library("plotly")
    library("shinyWidgets")
    library("shinyjs")
    library("shinyalert")
    library("shinydashboard")
    library("shinycssloaders")
    library("zoo")
    library("changepoint")
    library("mgcv")
    library("stats")
    library("utils")
    library("foreach")
    library("DT")
}

## Set token for Mapbox
Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoicnVnZWVyIiwiYSI6ImNqOTduN2phMTBmYXkyd29yNjR1amU2cjUifQ.IhNRZRmy1mlbLloz-p6vbw")
## Set the maximum file size to upload
options(shiny.maxRequestSize = 30 * 1024^3)

## Shiny server configuration
server <- function(input, output, session) {
    ## Ensure that when button for changepoints clicked, the window does not
    ## dissapear by a click in the window.
    shinyjs::runjs(
                 '$(document).on("click", ".dropdown-menu", function (e) {
    e.stopPropagation();
  });
    ')

    ## Main object where most data is stored
    data <- reactiveValues(
        summary = NULL, object = NULL,
        selectedSessions = NULL, hasData = NULL
    )
    ## Store the pervious value to let user upload new data constantly
    previous_file_paths <- reactiveValues(processed = 'NULL')

    ## Load named vectors
    choices <- trackeRapp:::choices()
    metrics <- trackeRapp:::metrics()

    ##  Upload data
    observeEvent(input$uploadButton, {
        no_raw_directory_selected <- is.null(input$rawDataDirectory$datapath)
        no_processed_file_selected <- is.null(input$processedDataPath$datapath)
        if (no_raw_directory_selected & no_processed_file_selected) {
            trackeRapp:::show_warning_no_data_selected()
        }
        else {
            processed_data <- raw_data <- NULL
            if (!no_processed_file_selected) {
                if (input$processedDataPath$datapath != previous_file_paths$processed) {
                                        # Load processed data
                    processed_data <- readRDS(input$processedDataPath$datapath)
                }
            }
            if (!no_raw_directory_selected) {
                from <- input$rawDataDirectory$datapath
                to <- file.path(dirname(from), basename(input$rawDataDirectory$name))
                file.rename(from, to)
                directory <- dirname(to[1])
                ## Process raw data
                raw_data <- trackeRapp:::read_directory_shiny(
                                             directory = directory,
                                             timezone = "GMT",
                                             parallel = TRUE,
                                             correct_distances = FALSE)
            }
            previous_file_paths$processed <- input$processedDataPath$datapath
            ## Process uploaded data
            ## Remove duplicate sessions and create trackeRdata object from both raw and processed data
            data$object <- sort(unique(trackeR:::c.trackeRdata(processed_data, raw_data,
                                                               data$object)), decreasing = FALSE)
            ## See helper file
            trackeRapp:::generate_objects(data, output, session, choices)
            trackeRapp:::update_sport_selection(data, session)
        }
    })

    ## Selected sessions
    proxy <- DT::dataTableProxy('summary')
    ## Sessions selected from plots using box/lasso selection
    observeEvent(plotly::event_data("plotly_selected"), {
        trackeRapp:::generate_selected_sessions_object(data, input,
                                                       plot_selection = TRUE)
        DT::selectRows(proxy = proxy, selected = data$selectedSessions)
    })

    ## Sessions selected by sport using radio buttons
    observeEvent(input$sports, {
        ## shinyjs::js$resetSelection()
        ## shinyjs::delay(1000,
        trackeRapp:::generate_selected_sessions_object(data, input, sport_selection = TRUE)
        ## )
        ## shinyjs::delay(1000,
        DT::selectRows(proxy = proxy, selected = data$selectedSessions)
        ## )
        ## Update metrics available based on sport selected
        has_data_sport <- lapply(data$summary[which(trackeR::get_sport(data$summary) %in% input$sports)],
                                 function(session_summaries) {
                                     !all(is.na(session_summaries) | session_summaries == 0)
                                 })
        selected_metrics <- c(input$metricsSelected[sapply(input$metricsSelected, function(x)
            has_data_sport[[x]]
        )])
        metrics_available_sport <- reactive({c(choices[sapply(choices, function(x)
            has_data_sport[[x]]
        )])
        })
        shinyjs::delay(1000,
                       shinyWidgets::updatePickerInput(session = session, inputId = 'metricsSelected',
                                                       selected = selected_metrics,
                                                       choices = metrics_available_sport()))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    ## Sessions selected through summary table
    observeEvent(input$summary_rows_selected,  {
        if (!isTRUE(setequal(input$summary_rows_selected, data$selectedSessions))) {
            shinyjs::js$resetSelection()
            ## shinyjs::delay(1000,
            trackeRapp:::generate_selected_sessions_object(data, input,
                                                           table_selection = TRUE)
            ## )
        }
    }, ignoreNULL = TRUE)

    ## Reset button clicked
    observeEvent(input$resetSelection, {
        trackeRapp:::update_sport_selection(data, session)
        shinyjs::js$resetSelection()
        trackeRapp:::generate_selected_sessions_object(data, input, no_selection = TRUE)
        DT::selectRows(proxy = proxy, selected = NULL)
    })

    ##  Uploading sample dataset
    observeEvent(input$uploadSampleDataset, {
        removeModal()
        filepath <- system.file('extdata/sample.rds', package = 'trackeRapp')
        data$object <- readRDS(filepath)
        ## See helper file
        trackeRapp:::generate_objects(data, output, session, choices)
    })

    ##  Change units
    observeEvent(input$showModalUnits, {
        if (!is.null(data$object))
            trackeRapp:::show_change_unit_window(data)
        else
            trackeRapp:::show_warning_window()
    })
    observeEvent(input$updateUnits, {
        data$object <- trackeRapp:::change_object_units(data, input, "object")
        data$summary <- trackeRapp:::change_object_units(data, input, "summary")
        data$limits <- trackeR::compute_limits(data$object, a = 0.1)
        removeModal()
    })


    ## Session summaries page
    observeEvent(input$createDashboard, {
        if (is.null(data$object))
            trackeRapp:::show_warning_window()
        else {
            output$timeline_plot <- plotly::renderPlotly({
                if (!is.null(data$summary))
                    trackeRapp:::plot_timeline(data$summary, session = data$selectedSessions)
            })
            ## Re-render all plots
            metrics_available <- reactive({c(choices[sapply(choices, function(x)
                data$hasData[[x]]
            )])
            })
            trackeRapp:::create_option_box(sport_options = data$identified_sports,
                                           metrics_available = metrics_available())

            ## Summary table
            trackeRapp:::create_summary_timeline_boxes()
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
            output$summary <- trackeRapp:::render_summary_table(data, input)

            ## Summary boxes
            trackeRapp:::create_summary_boxes()
            output$avgDistance_box <- trackeRapp:::render_summary_box("distance",
                                                                      "Average distance", data)
            output$avgDuration_box <- trackeRapp:::render_summary_box("duration",
                                                                      "Average duration", data)
            output$avgHeartRate_box <- trackeRapp:::render_summary_box("avgHeartRate",
                                                                       "Average heart rate", data)
            output$avgPace_box <- trackeRapp:::render_summary_box("avgPace",
                                                                  "Average pace", data)

            ## Map
            ## Check if there is internet connection
            has_internet_connection <- curl::has_internet()
            ## do not generate map if no location data for any of the sessions
            if ((any(data$is_location_data)) & (has_internet_connection)) {
                trackeRapp:::create_map()
                preped_route_map <- reactive({
                    sessions <- seq_along(data$object)[data$is_location_data]
                    route <- trackeR:::prepare_route(data$object,
                                                     session = sessions, threshold = FALSE)
                    route$SessionID <- sessions[route$SessionID]
                    list(route = route, sessions = sessions)
                })
                output$map <- plotly::renderPlotly({
                    trackeRapp:::plot_map(df = preped_route_map()$route,
                                          all_sessions = preped_route_map()$sessions,
                                          sumX = data$summary,
                                          colour_sessions = isolate(data$selectedSessions))
                })
                ## Update map based on current selection
                observeEvent(c(data$selectedSessions, input$is_collapse_box1) , {
                    try(
                        if (!is.null(input$is_collapse_box1)) {
                            if (input$is_collapse_box1 != 'block') {
                                sessions_rows <- which(preped_route_map()$route$SessionID %in% data$selectedSessions)
                                plot_df <- preped_route_map()$route[sessions_rows, ]
                                if (nrow(plot_df) != 0)
                                    trackeRapp:::update_map(session,
                                                            data, longitude = plot_df$longitude,
                                                            latitude = plot_df$latitude)
                                else
                                    trackeRapp:::update_map(session, data,
                                                            longitude = preped_route_map()$route$longitude,
                                                            latitude = preped_route_map()$route$latitude)
                            }
                        }, silent = FALSE)
                }, ignoreInit = TRUE, priority = -1)
                shinyjs::js$is_map_collapse()
            }

            ## Sessions summaries plots
            plot_dataframe <- reactive({
                trackeR:::fortify_trackeRdataSummary(data$summary, melt = TRUE)
            })
            ## Generate conditional plot for each metric irrespective of whether data available
            for (metric in c(choices)) {
                trackeRapp:::create_workout_plots(metric)
            }

            sapply(c(choices), function(i) {
                output[[paste0(i, "_plot")]] <- plotly::renderPlotly({
                    sessions_to_plot <- data$summary$session[get_sport(data$object) %in% input$sports]
                    trackeRapp:::plot_workouts(sumX = data$summary[sessions_to_plot],
                                               what = i,
                                               dat =  plot_dataframe(),
                                               sessions = data$selectedSessions,
                                               sports = trackeR::get_sport(data$object)[sessions_to_plot])
                })
            })
            ## Set to TRUE such that all plots are visible
            output$cond <- reactive({
                TRUE
            })
            outputOptions(output, "cond", suspendWhenHidden = FALSE)
            data$show_summary_plots <- TRUE
            sapply(c(choices), function(choice) {
                output[[choice]] <- reactive({
                    !isTRUE((choice %in% input$metricsSelected) & (data$show_summary_plots))
                })
                outputOptions(output, choice, suspendWhenHidden = FALSE)
            })
        }
    }, once = TRUE)


    ## Test which metrics have data
    have_data_metrics_selected <- reactive({
        !sapply(metrics, function(metric) {
            all(sapply(data$object[data$selectedSessions], {
                function(x) all((is.na(x[, metric])) | (x[, metric] == 0))
            }))
        })
    })

    ## Workouts analysis
    observeEvent(input$proceed, {
        removeModal()
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        ##  Time in zones
        trackeRapp:::create_zones_box(
                         inputId = "zonesMetricsPlot",
                         plotId = "zonesPlotUi",
                         choices = metrics[have_data_metrics_selected()]
                     )
        ## Render UI for time in zones plot
        output$zonesPlotUi <- renderUI({
            shiny::req(input$zonesMetricsPlot)
            shinycssloaders::withSpinner(plotly::plotlyOutput(
                                                     "zones_plot",
                                                     width = "100%",
                                                     height = trackeRapp:::calculate_plot_height(input$zonesMetricsPlot)), size = 2)
        })
        breaks <- reactive({
            compute_breaks(object = data$object, limits = data$limits,
                           n_breaks = as.numeric(input$n_zones),
                           what = input$zonesMetricsPlot)
        })
        ## Render actual plot
        output$zones_plot <- plotly::renderPlotly({
            trackeRapp:::plot_zones(x = data$object, session = data$selectedSessions,
                                    what = input$zonesMetricsPlot, breaks = breaks(),
                                    n_zones = as.numeric(input$n_zones))
        })
        ## Update metrics available each time different sessions selected
        observeEvent(data$selectedSessions, {
            shinyWidgets::updatePickerInput(session = session, inputId = "zonesMetricsPlot",
                                            choices =  metrics[have_data_metrics_selected()],
                                            selected = 'speed')
        }, ignoreInit = TRUE)

        ## Generate individual sessions plots (except work capacity)
        metrics_to_expand <- c('speed')

        ## First generate all plots irrespective if data available
        for (i in c(metrics)) {
            collapse <- if (i %in% metrics_to_expand) FALSE else TRUE
            i <- if (i == 'heart_rate') "heart_rate" else i
            trackeRapp:::create_selected_workout_plot(id = i, collapsed = collapse)
        }

        sapply(metrics, function(i) {
            plot_width <- reactive({
                ifelse(length(data$selectedSessions) > 3,
                       paste0(toString(500 * length(as.vector(data$selectedSessions))), "px"),
                       "auto")
            })
            output[[paste0(i, "_plot")]] <- renderUI({
                shinycssloaders::withSpinner(plotly::plotlyOutput(paste0(i, "Plot"),
                                                                  width = plot_width(),
                                                                  height = "250px"
                                                                  ), size = 2)
            })

            ## Render individual sessions plots (except work capacity)
            output[[paste0(i, "Plot")]] <- plotly::renderPlotly({
                ## Whether to detect changepoints
                if (!is.null(input[[paste0("detect_changepoints", i)]])) {
                    fit_changepoint <- input[[paste0("detect_changepoints", i)]] > 0
                }
                trackeRapp:::plot_selected_workouts(
                                 x = data$object, session = data$selectedSessions, what = i,
                                 sumX = data$summary, changepoints = fit_changepoint,
                                 threshold = FALSE, smooth = TRUE,
                                 n_changepoints = isolate(as.numeric(input[[paste0("n_changepoints", i)]])),
                                 desampling = 1, y_axis_range = data$limits[[i]])
            })

            output[[i]] <- reactive({
                !isTRUE((i %in% metrics[have_data_metrics_selected()]) & data$show_individual_sessions)
            })
            outputOptions(output, i, suspendWhenHidden = FALSE)
        })

        ##  Concentration profiles
        trackeRapp:::create_profiles_box(
                         inputId = "profileMetricsPlot",
                         plotId = "concentration_profiles",
                         choices = metrics[have_data_metrics_selected()],
                         collapsed = TRUE)
        ## Render UI for concentration profiles
        output$concentration_profiles <- renderUI({
            shiny::req(input$profileMetricsPlot)
            shinycssloaders::withSpinner(plotly::plotlyOutput(
                                                     "conc_profiles_plots",
                                                     width = "auto",
                                                     height = trackeRapp:::calculate_plot_height(input$profileMetricsPlot)), size = 2)
        })
        concentration_profiles <- reactive({
            trackeR::concentration_profile(data$object,
                                           what = metrics[have_data_metrics_selected()],
                                           limits = data$limits)
        })
        ## Render actual plot
        output$conc_profiles_plots <- plotly::renderPlotly({
            trackeRapp:::plot_concentration_profiles(
                             x = data$object,
                             session = data$selectedSessions,
                             what = input$profileMetricsPlot,
                             profiles_calculated = concentration_profiles())
        })

        ## Update metrics available each time different sessions selected
        observeEvent(data$selectedSessions, {
            shinyWidgets::updatePickerInput(session = session, inputId = "profileMetricsPlot",
                                            choices =  metrics[have_data_metrics_selected()],
                                            selected = 'speed')
        }, ignoreInit = TRUE)

        ## Generate work capacity plot
        ## Check which work capacity plots to generate
        work_capacity_ids <- reactive({
            trackeRapp:::test_work_capacity(data)
        })

        trackeRapp:::create_work_capacity_plot(id = 'work_capacity')

        sapply(c('cycling', 'running'), function(sport_id) {
            output[[paste0(sport_id, "_work_capacity_plot")]] <- renderUI({
                n_sessions <- sum(trackeR::get_sport(data$summary[data$selectedSessions]) %in% sport_id)
                plot_width <- ifelse(n_sessions > 3,
                                     paste0(toString(500 * n_sessions), "px"),
                                     "auto")
                shinycssloaders::withSpinner(plotly::plotlyOutput(paste0(sport_id, "Plot"),
                                                                  width = plot_width,
                                                                  height = "250px" ), size = 2)
            })

            ## Render work capacity
            output[[paste0(sport_id, "Plot")]] <- plotly::renderPlotly({
                ## If button to change units is pressed re-render plot with new units
                change_power[[sport_id]]
                work_capacity_sessions <- trackeR::get_sport(data$summary)[data$selectedSessions] %in% sport_id
                trackeRapp:::plot_work_capacity(
                                 x = data$object,
                                 session = data$selectedSessions[work_capacity_sessions],
                                 cp = isolate(as.numeric(input[[paste0('critical_power_', sport_id)]])))
            })
        })

        ## Conditions for displaying the work capacity plot
        output[[paste0('work_capacity_running')]] <- reactive({
            !isTRUE('running' %in%  work_capacity_ids())
        })

        output[[paste0('work_capacity_cycling')]] <- reactive({
            !isTRUE('cycling' %in%  work_capacity_ids())
        })

        output[['work_capacity']] <- reactive({
            !isTRUE((length(work_capacity_ids()) != 0) & data$show_work_capacity)
        })
        outputOptions(output, 'work_capacity_cycling', suspendWhenHidden = FALSE)
        outputOptions(output, 'work_capacity_running', suspendWhenHidden = FALSE)
        outputOptions(output, 'work_capacity', suspendWhenHidden = FALSE)

        ## Update power for work capacity plot
        change_power <- reactiveValues(cycling = 0, running = 0)
        observeEvent(input$cycling_update_power, {
            trackeRapp:::withBusyIndicatorServer("cycling_update_power", {
                Sys.sleep(1)
                if (!is.numeric(input$critical_power_cycling) | input$critical_power_cycling <= 0) {
                    stop("Invalid input. Input has to be a positive numeric value.")
                }
                else {
                    change_power$cycling <- change_power$cycling + 1
                }
            })
        })

        observeEvent(input$running_update_power, {
            trackeRapp:::withBusyIndicatorServer("running_update_power", {
                Sys.sleep(1)
                if (!is.numeric(input$critical_power_running) | input$critical_power_running <= 0) {
                    stop("Invalid input. Input has to be a positive numeric value.")
                }
                else {
                    change_power$running <- change_power$running + 1
                }
            })
        })
    }, once = TRUE)

    ## Toggle between session summaries page and individual sessions page
    observeEvent(input$return_to_main_page, {
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        output$cond <- reactive({
            TRUE
        })
        data$show_summary_plots <- TRUE
        data$show_individual_sessions <- FALSE
        data$show_work_capacity <- FALSE
    })

    observeEvent(input$proceed, {
        removeModal()
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        output$cond <- reactive({
            FALSE
        })
        data$show_summary_plots <- FALSE
        data$show_individual_sessions <- TRUE
        data$show_work_capacity <- TRUE
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    ##  Reset button
    observeEvent(input$resetButton, {
        shinyjs::js$reset_page()
    })

    ## Warning message too many sessions selected
    observeEvent(input$plotSelectedWorkouts, {
        nsessions <- length(data$selectedSessions)
        if (nsessions > 60) {
            show_warning_too_many_sessions(nsessions)
        }
        else {
            click("proceed")
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$proceed_modal, {
        click("proceed")
    })

}
