trops <- function() {
    custom_css <- ""
    ## custom_css <- "
    ## .btn-loading-container {
    ## margin-left: 10px;
    ## font-size: 1.2em;
    ## }
    ## .btn-done-indicator {
    ## color: green;
    ## }
    ## .btn-err {
    ## margin-top: 10px;
    ## color: red;
    ## }
    ## .warningMessage {
    ##    font-size: 20px;
    ## }
    ## hr {
    ## border-top: 1px solid;
    ## }
    ## a#download_data {
    ## color: #333;
    ## }
    ## .main_plots rect.legendtoggle {
    ## display: none;
    ## }
    ## .main_plots rect.legendtoggle {
    ## cursor: default;
    ## }
    ## "

    list(
        ## red, yellow, aqua, blue, light-blue, green, navy, teal, olive,
        ## lime, orange, fuchsia, purple, maroon, black.
        colour_summary_box_na = "olive",
        colour_summary_box_ok = "light-blue",
        DT_selected_colour = 'background-color: orange !important;',
        # blue, black, purple, green, red, yellow
        skin = "black",
        custom_css = custom_css
    )
}
