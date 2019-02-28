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

## trops
opts <- trackeRapp:::trops()

## Set token for Mapbox
Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoicnVnZWVyIiwiYSI6ImNqOTduN2phMTBmYXkyd29yNjR1amU2cjUifQ.IhNRZRmy1mlbLloz-p6vbw")
## Set the maximum file size to upload
options(shiny.maxRequestSize = 30 * 1024^3)

## Shiny server configuration
server <- function(input, output, session) {
    ## Ensure that when button for changepoints clicked, the window does not
    ## dissapear by a click in the window.
    shinyjs::runjs('$(document).on("click", ".dropdown-menu", function (e) {
                     e.stopPropagation();
                    });')
    ## Main object where most data is stored
    data <- reactiveValues(summary = NULL,
                           object = NULL,
                           selected_sessions = NULL,
                           has_data = NULL)
    ## Store the previous value to let user upload new data constantly
    previous_file_paths <- reactiveValues(processed = 'NULL')

    ## Load named vectors
    choices <- trackeRapp:::summary_view_features()
    metrics <- trackeRapp:::workout_view_features()

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
                    ## Load processed data
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
        }
    })

    ## Selected sessions
    proxy <- DT::dataTableProxy('summary')
    ## Sessions selected from plots using box/lasso selection
    observeEvent(plotly::event_data("plotly_selected"), {
        trackeRapp:::generate_selected_sessions_object(data, input,
                                                       plot_selection = TRUE)
        DT::selectRows(proxy = proxy, selected = data$selected_sessions)
    })

    observeEvent(input$all_sports, {
        data$sports <- c("running", "cycling", "swimming")
    })

    observeEvent(input$resetSelection, {
        data$sports <- NULL
    })

    observeEvent(input$sport_is_cycling, {
        data$sports <- "cycling"
    })

    observeEvent(input$sport_is_running, {
        data$sports <- "running"
    })

    observeEvent(input$sport_is_swimming, {
        data$sports <- "swimming"
    })

    ## Sessions selected by sport using selection panel.
    observeEvent(data$sports, {
        ## c(input$sport_is_cycling, input$sport_is_running, input$sport_is_swimming, input$all_sports), {
        shinyjs::delay(1000, trackeRapp:::generate_selected_sessions_object(data, input, sport_selection = TRUE))
        shinyjs::delay(1000,  DT::selectRows(proxy = proxy, selected = data$selected_sessions))
        ## update metrics available based on sport selected
        has_data_sport <- lapply(data$summary[which(trackeR::get_sport(data$summary) %in% data$sports)],
                                 function(session_summaries) {
                                     !all(is.na(session_summaries) | session_summaries == 0)
                                 })
        selected_metrics <- c(input$metricsSelected[sapply(input$metricsSelected, function(x)
            has_data_sport[[x]]
            )])

        ## The default value of selected metrics
        if (is.null(selected_metrics)) {
            selected_metrics <- opts$default_summary_plots
        }

        metrics_available_sport <- reactive({c(choices[sapply(choices, function(x)
            has_data_sport[[x]]
            )])
        })

        shinyjs::delay(100, shinyWidgets::updatePickerInput(session = session, inputId = 'metricsSelected',
                                                            selected = selected_metrics,
                                                            choices = metrics_available_sport()))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    ## Sessions selected through summary table
    observeEvent(input$summary_rows_selected,  {
        if (!isTRUE(setequal(input$summary_rows_selected, data$selected_sessions))) {
            shinyjs::js$resetSelection()
            trackeRapp:::generate_selected_sessions_object(data, input,
                                                           table_selection = TRUE)
        }
    }, ignoreNULL = FALSE)

    ## Reset button clicked
    ## observeEvent(input$resetSelection, {
    ##     shinyjs::js$resetSelection()
    ##     trackeRapp:::generate_selected_sessions_object(data, input, no_selection = TRUE)
    ##     DT::selectRows(proxy = proxy, selected = NULL)
    ## })

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
        trackeRapp:::show_change_unit_window(data)
    })

    observeEvent(input$updateUnits, {
        data$object <- trackeRapp:::change_object_units(data, input, "object")
        data$summary <- trackeRapp:::change_object_units(data, input, "summary")
        data$limits <- trackeR::compute_limits(data$object, a = 0.1)
        removeModal()
    })

    ## Session summaries page
    observeEvent(input$createDashboard, {
        output$timeline_plot <- plotly::renderPlotly({
            withProgress(message = 'Timeline', value = 0, {
                incProgress(1/1, detail = "Plotting")
                if (!is.null(data$summary))
                    trackeRapp:::plot_timeline(data$summary, session = data$selected_sessions)
            })
        })
        ## Re-render all plots
        metrics_available <- reactive({c(choices[sapply(choices, function(x)
            data$has_data[[x]]
            )])
        })
        trackeRapp:::create_option_box(sport_options = data$identified_sports,
                                       metrics_available = metrics_available())

        ## Check which sports are available in the data and disable the selectors accordingly
        sapply(c("running", "cycling", "swimming"), function(sp) {
            if (!(sp %in% data$identified_sports)) {
                shinyjs::disable(paste0("sport_is_", sp))
            }
        })

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
                withProgress(message = 'Map', value = 0, {
                    incProgress(1/2, detail = "Preparing routes")
                    pr <- preped_route_map()$sessions
                    incProgress(1/1, detail = "Mapping")
                    trackeRapp:::plot_map(df = preped_route_map()$route,
                                          all_sessions = pr,
                                          sumX = data$summary,
                                          colour_sessions = isolate(data$selected_sessions))
                })
            })

            ## Update map based on current selection
            observeEvent(c(data$selected_sessions, input$is_collapse_box1) , {
                try(if (!is.null(input$is_collapse_box1)) {
                        if (input$is_collapse_box1 != 'block') {
                            sessions_rows <- which(preped_route_map()$route$SessionID %in% data$selected_sessions)
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
                withProgress(message = paste(i, "plots"), value = 0, {
                    incProgress(1/1, detail = "Subsetting")
                    cdat <- plot_dataframe()
                    incProgress(1/1, detail = "Plotting")
                    sessions_to_plot <- data$summary$session[get_sport(data$object) %in% data$sports]
                    trackeRapp:::plot_workouts(sumX = data$summary[sessions_to_plot],
                                               what = i,
                                               dat =  cdat,
                                               sessions = data$selected_sessions,
                                               sports = trackeR::get_sport(data$object)[sessions_to_plot])
                })
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
    }, once = TRUE)

    ## Test which metrics have data
    have_data_metrics_selected <- reactive({
        !sapply(metrics, function(metric) {
            all(sapply(data$object[data$selected_sessions], {
                function(x) all((is.na(x[, metric])) | (x[, metric] == 0))
            }))
        })
    })

    ## Workouts analysis
    observeEvent(input$proceed, {
        removeModal()
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        ##  Time in zones
        trackeRapp:::create_zones_box(inputId = "zonesMetricsPlot",
                                      plotId = "zonesPlotUi",
                                      choices = metrics[have_data_metrics_selected()])
        ## Render UI for time in zones plot
        output$zonesPlotUi <- renderUI({
            req(input$zonesMetricsPlot)
            plotly::plotlyOutput("zones_plot",
                                 width = "100%",
                                 height = paste0(opts$workout_view_rel_height * length(input$zonesMetricsPlot), "vw"))
        })

        ## Render actual plot
        breaks <- reactive({
            trackeR::compute_breaks(object = data$object, limits = data$limits,
                                    n_breaks = as.numeric(input$n_zones),
                                    what = input$zonesMetricsPlot)
        })

        output$zones_plot <- plotly::renderPlotly({
            withProgress(message = 'Zones plots', value = 0, {
                incProgress(1/2, detail = "Computing breaks")
                br <- breaks()
                incProgress(2/2, detail = "Plotting")
                trackeRapp:::plot_zones(x = data$object, session = data$selected_sessions,
                                        what = input$zonesMetricsPlot, breaks = br,
                                        n_zones = as.numeric(input$n_zones))
            })
        })


        ## Update metrics available each time different sessions selected
        observeEvent(data$selected_sessions, {
            shinyWidgets::updatePickerInput(session = session, inputId = "zonesMetricsPlot",
                                            choices =  metrics[have_data_metrics_selected()],
                                            selected = 'speed')
        }, ignoreInit = TRUE)

        ## Generate individual sessions plots (except work capacity)
        metrics_to_expand <- "" #c('speed')

        ## First generate all plots irrespective if data available
        for (i in c(metrics)) {
            collapse <- if (i %in% metrics_to_expand) FALSE else TRUE
            i <- if (i == 'heart_rate') "heart_rate" else i
            trackeRapp:::create_selected_workout_plot(id = i, collapsed = collapse)
        }

        sapply(metrics, function(i) {
            plot_width <- reactive({
                n_sessions <- length(as.vector(data$selected_sessions))
                paste0(opts$workout_view_rel_width * n_sessions, "vw")
            })
            output[[paste0(i, "_plot")]] <- renderUI({
                plotly::plotlyOutput(paste0(i, "Plot"),
                                     width = plot_width(),
                                     height = paste0(opts$workout_view_rel_height, "vw"))
            })

            ## Render individual sessions plots (except work capacity)
            output[[paste0(i, "Plot")]] <- plotly::renderPlotly({
                withProgress(message = paste(i, 'plots'), value = 0, {
                    ## Whether to detect changepoints
                    if (!is.null(input[[paste0("detect_changepoints", i)]])) {
                        fit_changepoint <- input[[paste0("detect_changepoints", i)]] > 0
                    }
                    incProgress(1/1, detail = "Plotting")
                    trackeRapp:::plot_selected_workouts(
                                     x = data$object, session = data$selected_sessions, what = i,
                                     sumX = data$summary, changepoints = fit_changepoint,
                                     threshold = FALSE, smooth = TRUE,
                                     n_changepoints = isolate(as.numeric(input[[paste0("n_changepoints", i)]])),
                                     desampling = 1, y_axis_range = data$limits[[i]])
                })
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
            req(input$profileMetricsPlot)
            plotly::plotlyOutput("conc_profiles_plots",
                                 width = "100%",
                                 height = paste0(opts$workout_view_rel_height * length(input$profileMetricsPlot), "vw")) ## , "vw"))
        })

        concentration_profiles <- reactive({
            trackeR::concentration_profile(data$object,
                                           what = metrics[have_data_metrics_selected()],
                                           limits = data$limits)
        })

        ## Render actual plot
        output$conc_profiles_plots <- plotly::renderPlotly({
            withProgress(message = 'Concentration profiles', value = 0, {
                incProgress(1/2, detail = "Computng profiles")
                cps <- concentration_profiles()
                incProgress(1/1, detail = "Plotting")
                trackeRapp:::plot_concentration_profiles(
                                 x = data$object,
                                 session = data$selected_sessions,
                                 what = input$profileMetricsPlot,
                                 profiles_calculated = cps)
            })
        })

        ## Update metrics available each time different sessions selected
        observeEvent(data$selected_sessions, {
            shinyWidgets::updatePickerInput(session = session, inputId = "profileMetricsPlot",
                                            choices =  metrics[have_data_metrics_selected()],
                                            selected = 'speed')
        }, ignoreInit = TRUE)
    }, once = TRUE)

    ## Toggle between session summaries page and individual sessions page
    observeEvent(input$return_to_main_page, {
        ## shinyjs::enable("metricsSelected")
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        output$cond <- reactive({
            TRUE
        })
        data$show_summary_plots <- TRUE
        data$show_individual_sessions <- FALSE
        data$show_work_capacity <- FALSE
        ## Enable the choice of metrics when in Summary view
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
        nsessions <- length(data$selected_sessions)
        if (nsessions > 60) {
            trackeRapp:::show_warning_too_many_sessions(nsessions)
        }
        else {
            shinyjs::click("proceed")
        }
        ## Disable the choice of metrics when in Workouts view
        ## shinyjs::disable("metricsSelected")
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$proceed_modal, {
        shinyjs::click("proceed")
    })

}
