## Get options
opts <- trackeRapp:::trops()

##   Interface when trackeRapp is first loaded
ui <- tagList(
    tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
               @media screen and (min-width: 1280px){
                .container{
                    width: 1280px;
                }
               }"),
               tags$div(class = "container",
 shinydashboard::dashboardPage(
  title = "trackeRapp",
  skin = opts$skin,
  shinydashboard::dashboardHeader(
                      title = span(tagList(#icon("dashboard"),
                          img(src = "text_trackeRapp.svg", height = "60%"))),
                      ## tags$li(class = "dropdown",
                      ##         ),
                      tags$li(class = "dropdown",
                              a(icon("github"), "GitHub",
                                title = "GitHub",
                                href = "https://github.com/trackerproject/trackeRapp",
                                target = "_blank")),
                      tags$li(class = "dropdown",
                              a(icon("youtube"), "YouTube",
                                title = "YouTube channel",
                                href = "https://www.youtube.com/channel/UCY6y-pw8d1kek1WAIWiVhhw",
                                target = "_blank")),
                      tags$li(class = "dropdown",
                              a(icon("question"), "Help",
                                title = "Help",
                                href = "https://trackerproject.github.io/trackeRapp/",
                                target = "_blank"))),
                      ## title = span(tagList(img(src = "hex_trackeRapp.svg", width = "50"), "trackeRapp"))),
  shinydashboard::dashboardSidebar(
                      tags$style(HTML(opts$custom_css)),
                      shinydashboard::sidebarMenu(
                                          br(),
                                          fileInput("processedDataPath",
                                                    "Import processed data (rds)",
                                                    multiple = FALSE,
                                                    buttonLabel = icon("folder-open"),
                                                    accept = c(".rds")),
                                          fileInput("rawDataDirectory", "Add raw data (gpx tcx json db3)",
                                                    multiple = TRUE,
                                                    buttonLabel = icon("folder-open"),
                                                    accept = c(".gpx", ".tcx", ".db3", ".json")),
                                          div(class = "form-group shiny-input-container",
                                              splitLayout(
                                                  actionButton("uploadButton", "Load", icon("upload")),
                                                  actionButton("resetButton", "Restart", icon("eraser")),
                                                  cellWidths = c("50%", "50%"))),
                                          hr(),
                                          div(class = "form-group shiny-input-container",
                                              tags$label("Export processed data"),
                                              downloadButton("download_data", "Download")),
                                          hr(),
                                          div(class = "form-group shiny-input-container",
                                              div(div(id = "fleft", span(id = "important_text", "Help")),
                                                  div(id = "fright",
                                                      a(icon("question"),
                                                        title = "Help",
                                                        href = "https://trackerproject.github.io/trackeRapp/",
                                                        target = "_blank"),
                                                      a(icon("youtube"),
                                                        title = "YouTube channel",
                                                        href = "https://www.youtube.com/channel/UCY6y-pw8d1kek1WAIWiVhhw",
                                                        target = "_blank"))),
                                              br(),
                                              div(div(id = "fleft",
                                                      span(id = "important_text", "Licence")),
                                                  div(id = "fright",
                                                      a("GPL3", href = "https://www.gnu.org/licenses/gpl-3.0.en.html", target = "_blank"))),
                                              br(),
                                              div(div(id = "fleft",
                                                      span(id = "important_text", "Local R installation")),
                                                  div(id = "fright",
                                                      a(icon("home"),
                                                        title = "CRAN",
                                                        href = "https://cran.r-project.org/package=trackeRapp",
                                                        target = "_blank"))),
                                              br(),
                                              div(div(id = "fleft", span(id = "important_text", "Development and code")),
                                                  div(id = "fright",
                                                      a(icon("github"),
                                                        title = "GitHub",
                                                        href = "https://github.com/trackerproject/trackeRapp",
                                                        target = "_blank"))),
                                                      ## a(icon("envelope"),
                                                      ##   title = "Email",
                                                      ##   href = "mailto: trackerproject@outlook.com",
                                              ##   target="blank_"))),
                                              br(),
                                              br(),
                                              div(div(id = "fleft",
                                                      HTML(text='<a class="github-button btnside" href="https://github.com/trackeRproject/trackeRapp" data-icon="octicon-star" data-show-count="true" data-text="Star" aria-label="Star trackeRproject/trackeRapp on GitHub">Star</a>')),
                                                  div(id = "fright",
                                                      HTML(text='<a class="github-button btnside" href="https://github.com/trackeRproject/trackeRapp/issues" data-icon="octicon-issue-opened" data-show-count="true" data-text="Issues"  aria-label="Issue trackeRproject/trackeRapp on GitHub">Issue</a>'))),
                                              br(),
                                              br(),
                                              div(div(id = "fleft",
                                                      HTML(text='<a class="github-button btnside" href="https://github.com/trackeRproject/trackeRapp/fork" data-icon="octicon-repo-forked" data-show-count="true" aria-label="Fork trackeRproject/trackeRapp on GitHub">Fork</a>')),
                                                  div(id = "fright",
                                                      HTML(text='<a class="github-button btnside" href="https://github.com/trackeRproject/trackeRapp/subscription" data-icon="octicon-eye" data-show-count="true" aria-label="Watch trackeRproject/trackeRapp on GitHub">Watch</a>')))),
                                          br(),
                                          hr(),
                                          div(class = "form-group shiny-input-container",
                                              div(span(id = "important_text", "Authors"),
                                                  br(),
                                                  div(id = "fleft",
                                                      "Robin Horňák"),
                                                  div(id = "fright",
                                                      a(icon("github"), title = "GitHub", href = "https://github.com/rugeer", target = "_blank"),
                                                      a(icon("linkedin"), title = "LinkedIn", href = "https://www.linkedin.com/in/robin-hornak-221756108/", target = "_blank")),
                                                  br(),
                                                  div(id = "fleft",
                                                      "Ioannis Kosmidis"),
                                                  div(id = "fright",
                                                      a(icon("globe"), title = "Ioannis Kosmidis page", href = "http://www.ikosmidis.com", target = "_blank"),
                                                      a(icon("twitter"), title = "Twitter", href = "http://twitter.com/IKosmidis_", target = "_blank"),
                                                      a(icon("github"), title = "GitHub", href = "https://github.com/ikosmidis", target = "_blank"),
                                                      a(icon("envelope"), title = "Email", href= "mailto: ioannis.kosmidis@warwick.ac.uk", target="blank_"),
                                                      a(icon("linkedin"), title = "LinkedIn", href = "https://www.linkedin.com/in/kosmidis", target = "_blank")))),
                                          br(),
                                          hr(),
                                          ## A hack to cause event in generate_objects after
                                          ## Load data button is clicked
                                          shinyjs::hidden(actionLink("createDashboard", "")),
                                          shinyjs::hidden(actionLink("proceed", "")))),
  shinydashboard::dashboardBody(
                      img(id = "logo", src = "hex_trackeRapp.svg"),
                      ## shinyjs::hidden(div(id = "firefoxmessage",
                      ##                     div(id = "sad_text",
                      ##                         img(src = "text_trackeRapp.svg", width = "120em"),
                      ##                         "has difficulties rendering interactive plots in Firefox"),
                      ##                     div(id = "sad_icon",
                      ##                         icon("sad-tear")),
                      ##                     div(id = "sad_text",
                      ##                         "Please use another browser while we are investigating the issue"))),
                      shinyjs::useShinyjs(),
                      ## JS to include
                      ## Browser checks
                      tags$head(tags$script(src="bowser.min.js")),
                      tags$head(tags$script(src="buttons.js")),
                      shinyjs::extendShinyjs(text = opts$custom_js,
                                             functions = c("window.onbeforeunload",
                                                           "collapse", "reset_page", "no_sports")),
                      ## A hack to load the right js
                      mapdeck::mapdeckOutput(outputId = "dummy_map")
                      ))))

