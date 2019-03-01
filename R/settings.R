trops <- function() {
    custom_css <-
    "
    /* tidy up fileInput margins and padding */
    section.sidebar .shiny-input-container {
       margin-bottom: 0px;
       margin-top: 0px;
       padding-top: 0px;
    }

    #file1_progress {
     margin-bottom: 0px;
     margin-top: 100px;
     padding: 0px;
    }

    #uploadButton, #resetButton, #return_to_main_page, #plotSelectedWorkouts, #showModalUnits, #sport_is_running, #sport_is_cycling, #sport_is_swimming, #all_sports, #no_sports {
     width: 100%;
     margin: 0px 0px 0px 0px;
    }

    #download_data {
     width: 100%;
     color: #333; /* from .btn-default in css */
    }

    #summary tr.selected td, #summary td.selected {
     background-color: #3c8dbc !important;
    }

    #workout_view_plot {
     overflow-x:scroll;
     overflow-y:hidden;
    }

     .main_plots rect.legendtoggle {
       display: none;
     }

     .main_plots rect.legendtoggle {
       cursor: default;
     }
    "
    custom_js <-
    "
     shinyjs.collapse = function(boxid) {
     $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
     };
     shinyjs.reset_page = function() { location.reload(); };
     shinyjs.no_sports = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); };
     shinyjs.initialize_map_collapse = function() {
       Shiny.onInputChange('is_collapse_box1', 'none')
     };
     shinyjs.check_map_collapse = function() {
       $('#box1').css('display')
     };
     shinyjs.is_map_collapse = function() {
      $('#box1').parent().find('button').click(function(){
       Shiny.onInputChange('is_collapse_box1', $('#box1').css('display'))
     })
     };
    "

    list(
        ## blue, black, purple, green, red, yellow
        skin = "black",
        ## red, yellow, aqua, blue, light-blue, green, navy, teal, olive,
        ## lime, orange, fuchsia, purple, maroon, black.
        custom_css = custom_css,
        custom_js = custom_js,
        summary_box_na_colour = "navy", ## same as #001f3f
        summary_box_ok_colour = "blue", # light-blue: #3c8dbc blue: #0073b7
        summary_plots_selected_colour = "#0073b7",
        summary_plots_deselected_colour = "#d2d6de",
        zones_colours = c("#6fc1f1", "#095b8b"),
        workouts_background_colour = "black",
        workouts_changepoint_colour = "#0073b7", ## "grey"
        workouts_smoother_colour = "#0073b7",
        mapbox_default_style = "light",
        workout_view_rel_width = 60,
        workout_view_rel_height = 40,
        dropdown_button_size = "sm", ## see ?shinyWidget::dropdownButton
        default_summary_plots = c("distance", "duration", "avgPace"),
        threshold = FALSE
    )
}


## zones_colours = c("#994700", "#ffad66"), ## https://www.hexcolortool.com/#ff851a
## zones_colours = c("#276248", "#69c39b"), ## https://www.hexcolortool.com/#3d9970
