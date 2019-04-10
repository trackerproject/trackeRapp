## Live version configuration
## set to TRUE only for a live version
live_version <- FALSE
if (isTRUE(live_version)) {
    library("trackeR")
    library("zoo")
    library("foreach")
    library("mgcv")
    library("plotly")
    library("DT")
    library("changepoint")
    library("shiny")
    library("shinyjs")
    library("shinydashboard")
    library("shinyWidgets")
    library("mapdeck")
    library("stats")
    library("utils")
    library("V8")
}

## trops
opts <- trackeRapp:::trops()

## Set token for Mapbox
mapbox_key <- "pk.eyJ1IjoicnVnZWVyIiwiYSI6ImNqOTduN2phMTBmYXkyd29yNjR1amU2cjUifQ.IhNRZRmy1mlbLloz-p6vbw"
## Set the maximum file size to upload
options(shiny.maxRequestSize = 30 * 1024^3)

## Shiny server configuration
server <- function(input, output, session) {

    ## Ensure that when button for changepoints clicked, the window does not
    ## dissapear by a click in the window.
    shinyjs::runjs('$(document).on("click", ".dropdown-menu", function (e) {
                     e.stopPropagation();
                    });
                    window.onbeforeunload = function(e) {
                    e = e || window.event;
                    if (e) {
                     e.returnValue = "Sure?";
                    }
                    return "Sure";
                    };')

    ## Main object where most data is stored
    data <- reactiveValues(summary = NULL,
                           object = NULL,
                           selected_sessions = NULL,
                           has_data = NULL,
                           dummy = 0,
                           has_metrics = NULL)

    ## Store the previous value to let user upload new data constantly
    previous_file_paths <- reactiveValues(processed = 'NULL')

    ## Load named vectors
    summary_features <- trackeRapp:::summary_view_features()
    workout_features <- trackeRapp:::workout_view_features()

    ## Check if there is internet connection
    has_internet_connection <- curl::has_internet()

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
            ## FIXME: If processed_data is NULL show modal that container file has now data and reset
            data$object <- sort(unique(trackeR:::c.trackeRdata(processed_data, raw_data,
                                                               data$object)), decreasing = FALSE)

            ## Threshold?
            if (opts$threshold) {
                th <- trackeR::generate_thresholds()
                units <- get_units(data$object)
                th <- trackeR:::change_units.trackeRthresholds(th, variable = units$variable, unit = units$unit, sport = units$sport)
                data$object <- threshold(data$object,
                                         variable = th$variable,
                                         lower = th$lower,
                                         upper = th$upper,
                                         sport = th$sport)
            }

            ## See helpers.R file
            trackeRapp:::generate_objects(data, output, session, summary_features)
        }

        ## Close sidebar
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        shinyjs::hide("logo")
        shinyjs::hide("dummy_map")

    })

    has_data_sport <- reactive({
        lapply(data$summary[which(trackeR::get_sport(data$summary) %in% if(!is.null(data$sports)) data$sports else c("running", "cycling", "swimming"))],
               function(session_summaries) {
                   !all(is.na(session_summaries) | session_summaries == 0)
               })
    })

    metrics_available_sport <- reactive({
        c(summary_features[sapply(summary_features, function(x)
            has_data_sport()[[x]]
            )])
    })

    selected_metrics <- reactive({
      s <- c(input$metricsSelected[sapply(input$metricsSelected, function(x) has_data_sport()[[x]])])
      if (is.null(s)) {
        opts$default_summary_plots
      }
      else {
        s
      }
    })

    ## Selected sessions
    proxy <- DT::dataTableProxy('summary')

    ## Sessions selected from plots using box/lasso selection
    observeEvent(plotly::event_data("plotly_selected"), {
        trackeRapp:::generate_selected_sessions_object(data, input,
                                                       plot_selection = TRUE)
        DT::selectRows(proxy = proxy, selected = data$selected_sessions)
        shinyWidgets::updatePickerInput(session = session, inputId =
                                                               "'metricsSelected",
                                        selected = selected_metrics(),
                                        choices = metrics_available_sport())
    })

    observeEvent(input$all_sports, {
        data$sports <- c("running", "cycling", "swimming")
        data$dummy <- data$dummy + 1
    })

    observeEvent(input$no_sports, {
        data$sports <- NULL
        data$dummy <- data$dummy + 1
    })

    observeEvent(input$sport_is_cycling, {
        data$sports <- "cycling"
        data$dummy <- data$dummy + 1
    })

    observeEvent(input$sport_is_running, {
        data$sports <- "running"
        data$dummy <- data$dummy + 1
    })

    observeEvent(input$sport_is_swimming, {
        data$sports <- "swimming"
        data$dummy <- data$dummy + 1
    })

    ## Sessions selected by sport using selection panel.
    observeEvent(c(data$sports, data$dummy), {
        ## FIXME: Do we really need these delays here?
        shinyjs::delay(1000,
                       trackeRapp:::generate_selected_sessions_object(data, input, sport_selection = TRUE)
                       )
        shinyjs::delay(1000,
                       DT::selectRows(proxy = proxy, selected = data$selected_sessions)
                       )
        shinyWidgets::updatePickerInput(session = session, inputId = "metricsSelected",
                                        selected = selected_metrics(),
                                        choices = metrics_available_sport())
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    ## Sessions selected through summary table
    observeEvent(input$summary_rows_selected,  {
        if (!isTRUE(setequal(input$summary_rows_selected, data$selected_sessions))) {
            shinyjs::js$no_sports()
            trackeRapp:::generate_selected_sessions_object(data, input,
                                                           table_selection = TRUE)
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    ##  Uploading sample dataset
    observeEvent(input$uploadSampleDataset, {
        removeModal()
        filepath <- system.file("extdata/sample.rds", package = "trackeRapp")
        data$object <- readRDS(filepath)
        ## See helper file
        trackeRapp:::generate_objects(data, output, session, summary_features)
    })

    ##  Change units
    observeEvent(input$showModalUnits, {
        trackeRapp:::show_change_unit_window(data)
    })

    observeEvent(input$updateUnits, {
        data$object <- trackeRapp:::change_object_units(data, input, "object")
        data$summary <- trackeRapp:::change_object_units(data, input, "summary")
         DT::selectRows(proxy = proxy, selected = data$selected_sessions)
        removeModal()
    })

    ## Message and actions on firefox issues
    shinyjs::runjs('Shiny.setInputValue("browser", bowser.name);')
    observeEvent(input$browser, {
        if (grepl("firef", input$browser, ignore.case = TRUE)) {
            shinyjs::show(id = "firefoxmessage")
            shinyjs::disable(id = "resetButton")
            shinyjs::disable(id = "uploadButton")
            shinyjs::disable(id = "download_data")
        }
    })

    ## Session summaries page
    observeEvent(input$createDashboard, {

        output$timeline_plot <- plotly::renderPlotly({
            withProgress(message = "Timeline", value = 0, {
                if (!is.null(data$summary)) {
                    ret <- trackeRapp:::plot_timeline(data$summary, session = data$selected_sessions,
                                                      options = opts)
                }
                incProgress(1/1, detail = "Plotting")
                ret
            })
        })

        ## Re-render all plots
        metrics_available <- reactive({
            c(summary_features[sapply(summary_features, function(x) data$has_data[[x]] )])
        })

        trackeRapp:::create_option_box(sport_options = data$identified_sports,
                                       summary_features_available = metrics_available(),
                                       workout_features_available = workout_features)

        ## Check which sports are available in the data and disable the selectors accordingly
        sapply(c("running", "cycling", "swimming"), function(sp) {
            if (!(sp %in% data$identified_sports)) {
                shinyjs::disable(paste0("sport_is_", sp))
            }
        })

        ## Summary table
        trackeRapp:::create_summary_timeline_boxes()

        output$summary <- trackeRapp:::render_summary_table(data, input, options = opts)

        ## Summary boxes
        trackeRapp:::create_summary_boxes()
        output$nsessions_cycling_box <- trackeRapp:::render_summary_box("nsessions_cycling",
                                                                        "rides", data)
        output$nsessions_running_box <- trackeRapp:::render_summary_box("nsessions_running",
                                                                        "runs", data)
        output$nsessions_swimming_box <- trackeRapp:::render_summary_box("nsessions_swimming",
                                                                         "swims", data)
        output$avgDistance_box <- trackeRapp:::render_summary_box("distance",
                                                                  "Average distance", data)
        output$avgDuration_box <- trackeRapp:::render_summary_box("duration",
                                                                  "Average duration", data)
        output$avgHeartRate_box <- trackeRapp:::render_summary_box("avgHeartRate",
                                                                   "Average heart rate", data)
        output$avgPace_box <- trackeRapp:::render_summary_box("avgPace",
                                                              "Average pace", data)
        ## output$avgAltitude_box <- trackeRapp:::render_summary_box("avgAltitude",
        ##                                                           "Average altitude", data)
        output$total_elevation_gain_box <- trackeRapp:::render_summary_box("total_elevation_gain",
                                                                           "Total elevation gain", data)
        output$avgTemperature_box <- trackeRapp:::render_summary_box("avgTemperature",
                                                                     "Average Temperature", data)


        ## Map (move to a plot_map function)
        ## do not generate map if no location data for any of the sessions
        if ((any(data$is_location_data)) & (has_internet_connection)) {
            trackeRapp:::create_map()

            output$map <- mapdeck::renderMapdeck({
                mapdeck::mapdeck(token = mapbox_key,
                                 style = mapdeck::mapdeck_style(opts$mapdeck_style))
            })

            selected_data <- reactive({
                sessions <- seq_along(data$object)[data$is_location_data]
                sessions <- sessions[sessions %in% data$selected_sessions]
                if (length(sessions)) {
                    out <- trackeRapp:::get_coords(data, sessions = sessions, keep = opts$coordinates_keep)
                    out$col <- ifelse(out$sport == "running",
                                        opts$summary_plots_selected_colour_run,
                                 ifelse(out$sport == "cycling",
                                        opts$summary_plots_selected_colour_ride,
                                        opts$summary_plots_selected_colour_swim))
                    out$tooltip <- paste(out$tooltip)
                    out
                }
                else {
                    NULL
                }
            })

            deselected_data <- reactive({
                sessions <- seq_along(data$object)[data$is_location_data]
                sessions <- sessions[!(sessions %in% data$selected_sessions)]
                if (length(sessions)) {
                    out <- trackeRapp:::get_coords(data, sessions = sessions, keep = opts$coordinates_keep)
                    out$col <- ifelse(out$sport == "running",
                                        opts$summary_plots_selected_colour_run,
                                 ifelse(out$sport == "cycling",
                                        opts$summary_plots_selected_colour_ride,
                                    opts$summary_plots_selected_colour_swim))
                    out$tooltip <- paste(out$tooltip)
                    out
                }
                else {
                    NULL
                }
            })

            ## Selecting from the map
            ## observeEvent(input$map_path_click, {
            ##     js <- input$map_path_click
            ##     lind <- jsonlite::fromJSON(js)$index
            ##     cat(lind, "\n")
            ## })

            ## Update map based on current selection
            observeEvent(data$selected_sessions, {
                ## Profile memory usage
                ## print(pryr:::object_size(data))

                withProgress(message = 'Map', value = 0, {
                    sel <- selected_data()
                    des <- deselected_data()
                    incProgress(1/2, detail = "Preparing routes")
                    ## FIXME: mapdeck gets confused with the tooltips if we do not do the below

                    incProgress(1/1, detail = "Mapping")
                    if (!is.null(des)) {
                        p <- mapdeck::mapdeck_update(map_id = "map")
                        p <- mapdeck::clear_path(p, "deselection_path")
                        p <- mapdeck::add_path(p,
                                               data = des,
                                               stroke_colour = paste0(opts$summary_plots_deselected_colour, "80"),
                                               stroke_width = opts$mapdeck_width,
                                               layer_id = "deselection_path",
                                               update_view = FALSE,
                                               focus_layer = FALSE)
                    }
                    if (!is.null(sel)) {
                        p <- mapdeck::mapdeck_update(map_id = "map")
                        p <- mapdeck::clear_path(p, "selection_path")
                        p <- mapdeck::add_path(p,
                                               data = sel,
                                               stroke_colour = "col",
                                               stroke_width = opts$mapdeck_width * 2,
                                               tooltip = "tooltip",
                                               layer_id = "selection_path",
                                               update_view = TRUE,
                                               focus_layer = TRUE)
                        centroids <- sf::st_centroid(sel)
                        p <- mapdeck::add_screengrid(p,
                                                     data = centroids,
                                                     colour_range = rev(colorspace::sequential_hcl(h = 10, power = 1, c = 65, l = 70, n = 6)),
                                                     cell_size = 20,
                                                     opacity = 0.1)
                    }
                })
            }, priority = -3)
        }

        ## Sessions summaries plots
        ## Generate conditional plot for each metric irrespective of whether data available
        for (metric in c(summary_features)) {
            trackeRapp:::create_workout_plots(metric)
        }

        sapply(c(summary_features), function(i) {
            output[[paste0(i, "_plot")]] <- plotly::renderPlotly({
                withProgress(message = paste(i, "plots"), value = 0, {
                    incProgress(1/2, detail = "Subsetting")
                    ret <- trackeRapp:::plot_workouts(data = data,
                                                      what = i,
                                                      options = opts,
                                                      summary_type = "total")

                    incProgress(1/1, detail = "Plotting")
                    ret
                })
            })
        })

        ## Set to TRUE such that all plots are visible
        output$cond <- reactive({
            TRUE
        })

        outputOptions(output, "cond", suspendWhenHidden = FALSE)
        data$show_summary_plots <- TRUE
        sapply(c(summary_features), function(choice) {
            output[[choice]] <- reactive({
                !isTRUE((choice %in% input$metricsSelected) & (data$show_summary_plots))
            })
            outputOptions(output, choice, suspendWhenHidden = FALSE)
        })
    }, once = TRUE)

    ## Test which metrics have data
    metric_is_available <- reactive({
        if (length(data$selected_sessions)) {
            out <- colMeans(data$has_metrics[data$selected_sessions, workout_features, drop = FALSE]) > 0
        }
        else {
            out <- logical(length(workout_features))
        }
        names(out) <- names(workout_features)
        out
    })

    observeEvent(data$selected_sessions, {
        data$limits <- reactive({
            if (length(data$selected_sessions)) {
                trackeR::compute_limits(data$object[data$selected_sessions],
                                        a = opts$quantile_for_limits)
            }
            else {
                NULL
            }
        })
    })

    ## Workouts analysis
    observeEvent(input$proceed, {
        removeModal()
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")

        ##  Time in zones
        trackeRapp:::create_zones_box(inputId = "zonesMetricsPlot",
                                      plotId = "zonesPlotUi",
                                      choices = workout_features[metric_is_available()])

        ## Render UI for time in zones plot
        output$zonesPlotUi <- renderUI({
            req(input$zonesMetricsPlot)
            plotly::plotlyOutput("zones_plot",
                                 width = "auto",
                                 height = paste0(opts$workout_view_rel_height * length(input$zonesMetricsPlot), "vh"))
        })


        br <- reactive({
            lims <- data$limits()
            trackeR::compute_breaks(object = data$object,
                                    limits = lims,
                                    n_breaks = as.numeric(input$n_zones),
                                    what = input$zonesMetricsPlot)
        })

        ## Render actual plot
        output$zones_plot <- plotly::renderPlotly({
            withProgress(message = "Zones plots", value = 0, {
                incProgress(1/2, detail = "Computing")
                breaks <- br()
                ret <- trackeRapp:::plot_zones(x = data$object,
                                               session = data$selected_sessions,
                                               what = input$zonesMetricsPlot,
                                               breaks = breaks,
                                               n_zones = as.numeric(input$n_zones))
                incProgress(2/2, detail = "Plotting")
                ret
            })
        })
        ## Generate individual sessions plots (except work capacity)
        metrics_to_expand <- ""

        ## First generate all plots irrespective if data available
        for (i in workout_features) {
            collapse <- if (i %in% metrics_to_expand) FALSE else TRUE
            i <- if (i == "heart_rate") "heart_rate" else i
            trackeRapp:::create_selected_workout_plot(id = i,
                                                      workout_features = workout_features[metric_is_available()],
                                                      collapsed = collapse)
        }

        sapply(workout_features, function(i) {
            plot_width <- reactive({
                n_sessions <- length(as.vector(data$selected_sessions))
                paste0(opts$workout_view_rel_width * (n_sessions + 1), "vw")
            })
            output[[paste0(i, "_plot")]] <- renderUI({
                plotly::plotlyOutput(paste0(i, "Plot"),
                                     width = plot_width(),
                                     height = paste0(opts$workout_view_rel_height, "vw"))
            })

            ## Render individual sessions plots (except work capacity)
            output[[paste0(i, "Plot")]] <- plotly::renderPlotly({
                withProgress(message = paste(i, "plots"), value = 0, {
                    ## Whether to detect changepoints
                    fit_changepoint <- as.numeric(input[[paste0("n_changepoints", i)]]) > 0
                    ret <- trackeRapp:::plot_selected_workouts2(
                                            x = data$object,
                                            session = data$selected_sessions,
                                            what1 = i,
                                            what2 = input[[paste0("what2", i)]],
                                            changepoints = fit_changepoint,
                                            smooth = TRUE,
                                            n_changepoints = isolate(as.numeric(input[[paste0("n_changepoints", i)]])),
                                            k = 101,
                                            thin = opts$thin,
                                            ylim1 = data$limits()[[i]],
                                            ylim2 = data$limits()[[input[[paste0("what2", i)]]]])
               incProgress(1/1, detail = "Plotting")
                    ret
                })
            })
            output[[i]] <- reactive({
                !isTRUE((i %in% workout_features[metric_is_available()]) &
                        data$show_individual_sessions &
                        (i %in% input$workout_features_selected))
            })
            outputOptions(output, i, suspendWhenHidden = FALSE)
        })



        ##  Concentration profiles
        trackeRapp:::create_profiles_box(
                         inputId = "profileMetricsPlot",
                         plotId = "concentration_profiles",
                         choices = workout_features[metric_is_available()],
                         collapsed = TRUE)
        ## Render UI for concentration profiles
        output$concentration_profiles <- renderUI({
            req(input$profileMetricsPlot)
            plotly::plotlyOutput("conc_profiles_plots",
                                 width = "auto",
                                 height = paste0(opts$workout_view_rel_height * length(input$profileMetricsPlot), "vh"))
        })

        conc_profiles <- reactive({
                    wh <- workout_features[metric_is_available()]
                    if (length(wh)) {
                        lims <- data$limits()
                        trackeR::concentration_profile(data$object,
                                                       what = wh,
                                                       limits = lims)
                    }
                    else {
                        NULL
                    }
        })


        ## Render actual plot
        output$conc_profiles_plots <- plotly::renderPlotly({
            withProgress(message = "Training concentration", value = 0, {
                incProgress(1/2, detail = "Computing")

                ## Compute concentration for static limits on all data and
                ## then simply plot with reactive limits
                ret <- trackeRapp:::plot_concentration_profiles(
                                        x = data$object,
                                        session = data$selected_sessions,
                                        what = input$profileMetricsPlot,
                                        profiles_calculated = conc_profiles(),
                                        limits = data$limits(),
                                        options = opts)
                incProgress(1/1, detail = "Plotting")
                ret
            })
        })

        ## Update metrics available each time different sessions selected
        observeEvent(data$selected_sessions, {
            has_sessions <- isTRUE(length(data$selected_sessions) > 0)
            has_profile_metrics <- isTRUE(length(input$profileMetricsPlot) > 0)
            has_zone_metrics <- isTRUE(length(input$zonesMetricsPlot) > 0)
            profile_metrics <- if (has_sessions & !has_profile_metrics) "speed" else input$profileMetricsPlot
            zone_metrics <- if (has_sessions & !has_zone_metrics) "speed" else input$zonesMetricsPlot
            shinyWidgets::updatePickerInput(session = session, inputId = "zonesMetricsPlot",
                                            choices =  workout_features[metric_is_available()],
                                            selected = zone_metrics)
            shinyWidgets::updatePickerInput(session = session, inputId = "profileMetricsPlot",
                                            choices =  workout_features[metric_is_available()],
                                            selected = profile_metrics)
        }, ignoreInit = TRUE)

    }, once = TRUE)

    ## Toggle between summary view and workout view
    observeEvent(input$return_to_main_page, {
        ## shinyjs::enable("metricsSelected")
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
        nsessions <- length(data$selected_sessions)
        if (isTRUE(nsessions > 60)) {
            trackeRapp:::show_warning_too_many_sessions(nsessions)
        }
        else {
            shinyjs::click("proceed")
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$proceed_modal, {
        shinyjs::click("proceed")
    })

}
