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

    #uploadButton, #resetButton, #return_to_main_page, #plotSelectedWorkouts, #showModalUnits, #resetSelection {
     width: 100%;
     margin: 0px 0px 0px 0px;
    }

    #download_data {
     width: 100%;
     color: #333; /* from .btn-default in css */
    }

    #summary tr.selected td, #summary td.selected {
     background-color: #ff851b !important; /* #FFE9D0 */
    }

    #workout_view_plot {
     overflow-x:scroll;
     overflow-y:hidden;
    }
    "
    custom_js <-
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

    list(
        ## blue, black, purple, green, red, yellow
        skin = "black",

        ## red, yellow, aqua, blue, light-blue, green, navy, teal, olive,
        ## lime, orange, fuchsia, purple, maroon, black.
        custom_css = custom_css,
        custom_js = custom_js,
        ## summary_box_na_colour = "olive", ## same as #3d9970x
        summary_box_na_colour = "light-blue", ## same as #3c8dbc
        summary_box_ok_colour = "orange", ## same as #ff851b
        summary_plots_selected_colour = "#ff851b", ## same as summary_box_ok_colour,
        summary_plots_deselected_colour = "#3c8dbc", ## same as summary_box_na_colour,
        zones_colours = c("#994700", "#ffad66"), ## https://www.hexcolortool.com/#ff851a
        ## zones_colours = c("#276248", "#69c39b"), ## https://www.hexcolortool.com/#3d9970
        workouts_background_colour = "black",
        workouts_changepoint_colour = "grey",
        workouts_smoother_colour = "#ff851b",
        mapbox_default_style = "light",
        workout_view_rel_width = 40,
        workout_view_rel_height = 20
    )
}