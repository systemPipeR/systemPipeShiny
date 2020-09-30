## UI
#' @importFrom shinydashboardPlus carousel carouselItem
#' @noRd
core_dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Welcome to systemPipeShiny"),
        h2("Introduction"),
        p("systemPipeShiny (SPS) extends the widely used systemPipeR (SPR)
          workflow environment with a versatile graphical user interface
          provided by a Shiny App. This allows non-R users, such as
          experimentalists, to run many systemPipeR’s workflow designs,
          control, and visualization functionalities interactively without
          requiring knowledge of R. Most importantly, SPS has been designed
          as a general purpose framework for interacting with other R packages
          in an intuitive manner. Like most Shiny Apps, SPS can be used on both
          local computers as well as centralized server-based deployments that
          can be accessed remotely as a public web service for using SPR’s
          functionalities with community and/or private data."),
        spsHr(),
        h2("To start"),
        p("Start to use the app by choosing 'Workflow Management' or
          'Visualization' from the left side navigation bar. They are the two
          major components of SPS. Clicking them will redirect you to the main
          instructions of these functionalities."),
        spsHr(),
        h2("Vignette"),
        HTML('<p>There is a <a href="https://systempipe.org/systemPipeShiny/articles/systemPipeShiny.html">vignette</a> to provide more
             details. If you are new to SPS, try our <a href="https://tgirke.shinyapps.io/systemPipeShiny/">demo</a> and click on
             the top right corner for interactive tutorials.</p>'),
        fluidRow(
            shinydashboardPlus::carousel(width = 12,
                id = "dashcarousel",
                shinydashboardPlus::carouselItem(
                    caption = "SPS",
                    tags$img(style = "width: 500px;",
                             src = "img/sps.png",
                             class = "center-block")
                ),
                shinydashboardPlus::carouselItem(
                    caption = "SPS Structure",
                    tags$img(style = "width: 500px;",
                             src = "img/sps_structure.png",
                             class = "center-block")
                )
            )
        ),

    )
}

## server
core_dashboardServer <- function(id, shared){
    module <- function(input, output, session, shared){
        ns <- session$ns
    }
    moduleServer(id, module)
}
