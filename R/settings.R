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

    #summary tr.selected.running td, #summary td.selected.running {
     background-color: #2AC28A !important;
    }

    #summary tr.selected.cycling td, #summary td.selected.cycling {
     background-color: #CEA550 !important;
    }

    #summary tr.selected.swimming td, #summary td.selected.swimming {
     background-color: #67B2EB !important;
    }

    .small-box.bg-blue {
     background-color: #00BFD0 !important;
    }

    .small-box.bg-navy {
     background-color: #d2d6de !important;
    }

    .small-box.bg-light-blue {
     background-color: #67B2EB !important;
    }

    .small-box.bg-yellow {
     background-color: #CEA550 !important;
    }

    .small-box.bg-green {
     background-color: #2AC28A !important;
    }

    .logo {
     background-color: #fff !important;
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

     #logo {
       width: 50%;
       display: block;
       margin-left: auto;
       margin-right: auto;
     }

     #sad_text {
       font-size: 1.5em;
       text-align: center;
       color: #222d32;
     }

     #sad_icon {
       font-size: 3em;
       text-align: center;
       color: #222d32;
     }

     #important_text {
       font-weight: bold;
     }

    #fright {
      float: right;
    }

    #fleft {
      float: left;
    }
    "
    custom_js <-
    "
     shinyjs.collapse = function(boxid) {
     $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
     };
     shinyjs.reset_page = function() { location.reload(); };
     shinyjs.no_sports = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); };
    "

    dt_callback_js <-
    "
     function(){
      $('tr').each(function(){
       var col_val = $(this).find('td:eq(6)').text();
        if (col_val == 'running'){
         $(this).addClass('running');
        };
        if (col_val == 'cycling'){
         $(this).addClass('cycling');
        };
        if (col_val == 'swimming'){
          $(this).addClass('swimming');
        }
      });
    }
    "

    list(
        ## blue, black, purple, green, red, yellow
        skin = "black",
        ## red, yellow, aqua, blue, light-blue, green, navy, teal, olive,
        ## lime, orange, fuchsia, purple, maroon, black.
        custom_css = custom_css,
        custom_js = custom_js,
        dt_callback_js = dt_callback_js,
        summary_box_na_colour = "navy", # same as #001f3f
        summary_box_ok_colour = "blue", # blue: #0073b7
        summary_box_swim_colour = "light-blue",
        summary_box_run_colour = "green",
        summary_box_ride_colour = "yellow",
        summary_plots_selected_colour = "#0073b7",
        summary_plots_deselected_colour = "#d2d6de",
        ## Pallete harmonic with h1 = 60, h2 = 240, c = 65, l = 70
        ##  qualitative_hcl(h1 = 60, h2 = 240, c = 65, l = 70, n = 3)
        ##  for average box qualitative_hcl(h1 = 10, h2 = 200, c = 65, l = 70, n =3)
        summary_plots_selected_colour_swim = "#67B2EB",
        summary_plots_selected_colour_run = "#2AC28A",
        summary_plots_selected_colour_ride = "#CEA550",
        summart_plots_yaxis_max_factor = 1.2,
        summart_plots_yaxis_min_factor = 0.8,
        zones_colours = list(swimming = c("#9AE5FF", "#347FB8"),
                             running = c("#5DF5BD", "#008F57"),
                             cycling = c("#FFD883", "#9B721D")),
        workouts_background_colour = "black",
        workouts_changepoint_colour = "#0073b7", ## "grey"
        workouts_smoother_colour = "#0073b7",
        mapdeck_style = "light",
        mapdeck_highlight_radius = 2000,
        mapdeck_width = 5,
        coordinates_keep = 0.2,
        thin = 0.5,
        workout_view_rel_width = 60,
        workout_view_rel_height = 30,
        dropdown_button_size = "sm", ## see ?shinyWidget::dropdownButton
        default_summary_plots = c("distance", "duration", "avgPace", "total_elevation_gain"),
        default_workout_plots = c("speed"),
        threshold = FALSE,
        quantile_for_limits = 0.05
    )
}


## zones_colours = c("#994700", "#ffad66"), ## https://www.hexcolortool.com/#ff851a
## zones_colours = c("#276248", "#69c39b"), ## https://www.hexcolortool.com/#3d9970

## 100% — FF
## 99% — FC
## 98% — FA
## 97% — F7
## 96% — F5
## 95% — F2
## 94% — F0
## 93% — ED
## 92% — EB
## 91% — E8
## 90% — E6
## 89% — E3
## 88% — E0
## 87% — DE
## 86% — DB
## 85% — D9
## 84% — D6
## 83% — D4
## 82% — D1
## 81% — CF
## 80% — CC
## 79% — C9
## 78% — C7
## 77% — C4
## 76% — C2
## 75% — BF
## 74% — BD
## 73% — BA
## 72% — B8
## 71% — B5
## 70% — B3
## 69% — B0
## 68% — AD
## 67% — AB
## 66% — A8
## 65% — A6
## 64% — A3
## 63% — A1
## 62% — 9E
## 61% — 9C
## 60% — 99
## 59% — 96
## 58% — 94
## 57% — 91
## 56% — 8F
## 55% — 8C
## 54% — 8A
## 53% — 87
## 52% — 85
## 51% — 82
## 50% — 80
## 49% — 7D
## 48% — 7A
## 47% — 78
## 46% — 75
## 45% — 73
## 44% — 70
## 43% — 6E
## 42% — 6B
## 41% — 69
## 40% — 66
## 39% — 63
## 38% — 61
## 37% — 5E
## 36% — 5C
## 35% — 59
## 34% — 57
## 33% — 54
## 32% — 52
## 31% — 4F
## 30% — 4D
## 29% — 4A
## 28% — 47
## 27% — 45
## 26% — 42
## 25% — 40
## 24% — 3D
## 23% — 3B
## 22% — 38
## 21% — 36
## 20% — 33
## 19% — 30
## 18% — 2E
## 17% — 2B
## 16% — 29
## 15% — 26
## 14% — 24
## 13% — 21
## 12% — 1F
## 11% — 1C
## 10% — 1A
## 9% — 17
## 8% — 14
## 7% — 12
## 6% — 0F
## 5% — 0D
## 4% — 0A
## 3% — 08
## 2% — 05
## 1% — 03
## 0% — 00

## valid box xolours
## red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
