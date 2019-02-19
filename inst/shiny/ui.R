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
      div(
        fileInput(
          "processedDataPath", "Choose processed file",
          multiple = FALSE,
          accept = c(".rds")
        )
      ),
      div(
        fileInput(
          "rawDataDirectory", "Choose raw data files",
          multiple = TRUE,
          accept = c(".gpx", ".tcx", ".db3", ".json")
        )
      ),
      actionButton("uploadButton", "Load data", icon("upload"),
                   style = "color: #fff; background-color: #6FB1E7; border-color: #5093E3"),
      hr(),
      actionButton("createDashboard", "Create Dashboard",
                   icon("area-chart"),
                   style = "color: #fff; background-color: #4FBF85; border-color: #00AB66",
                   width = "auto"
      ),
      hr(),
      div(
        class = "form-group shiny-input-container", tags$label("Download data"),
        div(class = "input-group", downloadButton("download_data", "Download procesed data"))
      ),
      hr(),
      div(style = "display: inline-block;vertical-align:top; width: 100px;",
          actionButton("resetButton", "Reset", icon("eraser"),
                       style = "color: #fff; background-color: #ED90A4; border-color: #E16A86",
                       width = "80px")),
      hr()
    ),
    div(
      class = "form-group shiny-input-container",
      p("Design and original development",
        br(),
        a("Robin Hornak"),
        br(),
        a("Ioannis Kosmidis", href = "http://www.ikosmidis.com", target="_blank")),
      p("Licence",
        br(),
        a("GPL3", href = "https://www.gnu.org/licenses/gpl-3.0.en.html", target="_blank")),
      p("Bugs, issues, feature requests",
        br(),
        a("github.com/trackerproject",
          href = "https://github.com/trackerproject/trackeRapp/issues",
          target="_blank")),
      p("Credits",
        br(),
        "swimmer/cyclist icons",
        a("(CC 3.0 BY)",
          href="http://creativecommons.org/licenses/by/3.0/",
          title="Creative Commons BY 3.0",
          target="_blank"),
        "by",
        a("Scott de Jonge", href="https://www.flaticon.com/authors/scott-de-jonge", title="Scott de Jonge"),
        br(),
        "runner icon",
        a("(Flaticon licence)",
          href = "https://file000.flaticon.com/downloads/license/license.pdf",
          title = "Flaticon basic licence",
          target="_blank"),
        "by",
        a("Freepic", href="https://www.flaticon.com/authors/freepik", title="Freepic")
        )
    )
),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shinyjs::extendShinyjs(text = trackeRapp:::get_javascript())
  )
)
