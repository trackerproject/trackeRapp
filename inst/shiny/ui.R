## Get options
opts <- trackeRapp:::trops()

##   Interface when trackeRapp is first loaded
ui <- shinydashboard::dashboardPage(
  title = "trackeRapp",
  skin = opts$skin,
  shinydashboard::dashboardHeader(
                      title = span(tagList(icon("dashboard"), "trackeRapp"))),
  shinydashboard::dashboardSidebar(
                      tags$style(HTML(opts$custom_css)),
                      shinydashboard::sidebarMenu(
                                          br(),
                                          fileInput("processedDataPath",
                                                    "Choose processed file",
                                                    multiple = FALSE,
                                                    accept = c(".rds")),
                                          fileInput("rawDataDirectory", "Choose raw data files",
                                                    multiple = TRUE,
                                                    accept = c(".gpx", ".tcx", ".db3", ".json")),
                                          div(class = "form-group shiny-input-container",
                                              splitLayout(
                                                  actionButton("uploadButton", "Load", icon("upload")),
                                                  actionButton("resetButton", "Reset", icon("eraser")),
                                                  cellWidths = c("50%", "50%"))),
                                          br(),
                                          div(class = "form-group shiny-input-container",
                                              tags$label("Download processed data"),
                                              downloadButton("download_data", "Download")),
                                          hr(),
                                          div(class = "form-group shiny-input-container",
                                              p("Design and original development",
                                                br(),
                                                a("Robin Hornak"),
                                                a(", "),
                                                a("Ioannis Kosmidis", 
                                                  href = "http://www.ikosmidis.com",
                                                  target = "_blank")),
                                              p("Manual",
                                                br(),
                                                a("Tour de TrackeRapp", 
                                                  href = "https://trackerproject.github.io/trackeRapp/", 
                                                  target = "_blank")),
                                              p("Licence",
                                                br(),
                                                a("GPL3", href = "https://www.gnu.org/licenses/gpl-3.0.en.html", target = "_blank")),
                                              p("Bugs, issues, feature requests",
                                                br(),
                                                a("github.com/trackerproject",
                                                  href = "https://github.com/trackerproject/trackeRapp/issues",
                                                  target = "_blank"))),
                                          ## A hack to cause event in generate_objects after
                                          ## Load data button is clicked
                                          shinyjs::hidden(actionLink("createDashboard", "")),
                                          shinyjs::hidden(actionLink("proceed", "")))),
  shinydashboard::dashboardBody(shinyjs::useShinyjs(),
                                shinyjs::extendShinyjs(text = opts$custom_js)))
