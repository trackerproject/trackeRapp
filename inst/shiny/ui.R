#   ____________________________________________________________________________
#   Interface when trackeRdashboard is first loaded                         ####

##  ............................................................................
##  User interface                                                          ####
ui <- shinydashboard::dashboardPage(
  title = "trackeRapp",
  skin = "black",
  shinydashboard::dashboardHeader(title = span(tagList(icon("dashboard"), "trackeRapp"))),
  shinydashboard::dashboardSidebar(
    tags$head(tags$style(trackeRapp:::appCSS)),
    shinydashboard::sidebarMenu(
      br(),
      fileInput("processedDataPath", "Choose processed file",
                multiple = FALSE,
                accept = c(".rds")),
      fileInput("rawDataDirectory", "Choose raw data files",
                multiple = TRUE,
                accept = c(".gpx", ".tcx", ".db3", ".json")),
      div(class = "form-group shiny-input-container",
          splitLayout(
              actionButton("uploadButton", "Load", icon("upload"),
                           style = "width: 100%; margin: 0px 0px 0px 0px"),
              ## style = "color: #fff; background-color: #6FB1E7; border-color: #5093E3"),
              actionButton("resetButton", "Reset", icon("eraser"),
                           style = "width: 100%; margin: 0px 0px 0px 0px"),
              cellWidths = c("50%", "50%")
          )),

      ## style = "color: #fff; background-color: #ED90A4; border-color: #E16A86",
      ## hr(),
      ## actionButton("createDashboard", "Create Dashboard",
      ##              icon("area-chart"),
      ##              style = "color: #fff; background-color: #4FBF85; border-color: #00AB66",
      ##              width = "auto"
      ## ),
      br(),
      div(class = "form-group shiny-input-container", tags$label("Download processed data"),
          downloadButton("download_data", "Download", style = "width: 100%")),
      hr(),
      ## div(style = "display: inline-block;vertical-align:top; width: 100px;",
      div(class = "form-group shiny-input-container",
          p("Design and original development",
            br(),
            a("Robin Hornak"),
            a(", "),
            a("Ioannis Kosmidis", href = "http://www.ikosmidis.com", target="_blank")),
          p("Licence",
            br(),
            a("GPL3", href = "https://www.gnu.org/licenses/gpl-3.0.en.html", target="_blank")),
          p("Bugs, issues, feature requests",
            br(),
            a("github.com/trackerproject",
              href = "https://github.com/trackerproject/trackeRapp/issues",
              target="_blank"))
      ),
      ## A hack to cause event in generate_objects after Load data button is clicked
      actionLink("createDashboard", ""),
    shinyjs::inlineCSS(list(
                 "section.sidebar .shiny-input-container" = "margin-bottom: 0px; margin-top: 0px; padding-top: 0px;",
                 "#file1_progress" = "margin-bottom: 0px; margin-top: 0px; padding: 0px;"))
    )
    ),
  shinydashboard::dashboardBody(
                      shinyjs::useShinyjs(),
                      shinyalert::useShinyalert(),
                      shinyjs::extendShinyjs(text = trackeRapp:::get_javascript())
                  )
  )

## Remove redundant line from fileInput buttons
