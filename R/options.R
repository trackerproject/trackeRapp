trops <- function() {
    custom_css <-
    "
    /* tidy up fileInput margins and padding
    section.sidebar .shiny-input-container {
       margin-bottom: 0px;
       margin-top: 0px;
       padding-top: 0px;
    }
    #file1_progress {
     margin-bottom: 0px;
     margin-top: 0px;
     padding: 0px;
    }
    "
    ## custom_css <-
    ## "
    ##  .btn-loading-container {
    ##  margin-left: 10px;
    ##  font-size: 1.2em;
    ##  }
    ##  .btn-done-indicator {
    ##  color: green;
    ##  }
    ##  .btn-err {
    ##  margin-top: 10px;
    ##  color: red;
    ##  }
    ##  .warningMessage {
    ##     font-size: 20px;
    ##  }
    ##  hr {
    ##  border-top: 1px solid;
    ##  }
    ##  a#download_data {
    ##  color: #333;
    ##  }
    ##  .main_plots rect.legendtoggle {
    ##  display: none;
    ##  }
    ##  .main_plots rect.legendtoggle {
    ##  cursor: default;
    ##  }
    ## "
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
        ## red, yellow, aqua, blue, light-blue, green, navy, teal, olive,
        ## lime, orange, fuchsia, purple, maroon, black.
        colour_summary_box_na = "olive",
        colour_summary_box_ok = "light-blue",
        DT_selected_colour = 'background-color: orange !important;',
        # blue, black, purple, green, red, yellow
        skin = "black",
        custom_css = custom_css,
        custom_js = custom_js
    )
}
