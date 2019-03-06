## Get options
opts <- trackeRapp:::trops()

##   Interface when trackeRapp is first loaded
ui <- shinydashboard::dashboardPage(
  title = "trackeRapp",
  skin = opts$skin,
  shinydashboard::dashboardHeader(
                      title = span(tagList(#icon("dashboard"),
                                           img(src = "text_trackeRapp.svg", width = "60%")))),
                      ## title = span(tagList(img(src = "hex_trackeRapp.svg", width = "50"), "trackeRapp"))),
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
                                              p(span(id = "important_text", "Design and development"),
                                                br(),
                                                "Robin Horňák",
                                                a(icon("github"), href = "https://github.com/rugeer", target = "_blank"),
                                                a(icon("linkedin"), href = "https://www.linkedin.com/in/robin-hornak-221756108/", target = "_blank"),
                                                br(),
                                                "Ioannis Kosmidis",
                                                a(icon("globe"), href = "http://www.ikosmidis.com", target = "_blank"),
                                                a(icon("twitter"), href = "http://twitter.com/IKosmidis_", target = "_blank"),
                                                a(icon("github"), href = "https://github.com/ikosmidis", target = "_blank"),
                                                a(icon("envelope"), href= "mailto: ioannis.kosmidis@warwick.ac.uk", target="blank_"),
                                                a(icon("linkedin"), href = "https://www.linkedin.com/in/kosmidis", target = "_blank")),
                                              p(span(id = "important_text", "Licence"),
                                                a("GPL3", href = "https://www.gnu.org/licenses/gpl-3.0.en.html", target = "_blank")),
                                              p(span(id = "important_text", "Wishes, issues, stars"),
                                                a(icon("github"),
                                                  href = "https://github.com/trackerproject/trackeRapp",
                                                  target = "_blank"),
                                                a(icon("envelope"),
                                                  href = "mailto: trackerproject@outlook.com",
                                                  target="blank_")),
                                              p(#icon("question"),
                                                span(id = "important_text", "Help"),
                                                a(icon("question"),
                                                  href = "https://trackerproject.github.io/trackeRapp/",
                                                  target = "_blank"),
                                                a(icon("youtube"),
                                                  href = "https://www.youtube.com/channel/UCY6y-pw8d1kek1WAIWiVhhw",
                                                  target = "_blank"))),
                                          ## A hack to cause event in generate_objects after
                                          ## Load data button is clicked
                                          shinyjs::hidden(actionLink("createDashboard", "")),
                                          shinyjs::hidden(actionLink("proceed", "")))),
  shinydashboard::dashboardBody(
                      img(id = "logo", src = "hex_trackeRapp.svg"),
                      shinyjs::hidden(div(id = "firefoxmessage",
                                          div(id = "sad_text",
                                              img(src = "text_trackeRapp.svg", width = "120em"),
                                              "has difficulties rendering interactive plots in Firefox"),
                                          div(id = "sad_icon",
                                              icon("sad-tear")),
                                          div(id = "sad_text",
                                              "Please use another browser while we are investigating the issue"))),
                      shinyjs::useShinyjs(),
                      tags$head(tags$script(src="bowser.min.js")),
                      shinyjs::extendShinyjs(text = opts$custom_js)
                      ))


