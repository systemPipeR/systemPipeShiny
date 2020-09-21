## UI
#' @importFrom shinydashboardPlus carousel carouselItem
#' @noRd
core_dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("systemPipeShiny Welcome"),
        h2("Introduction"),
        p("systemPipeShiny(SPS) can help on data analysis workflow management
        and downstream data visualizations"),
        spsHr(),
        p("Start to use the app by choosing 'Workflow Management' or
          'Visualization' from the left side navigation bar."),
        spsHr(),
        h2("Vignette"),
        HTML('<p>There is a <a href="https://systempipe.org/systemPipeShiny/
             articles/systemPipeShiny.html">vignette</a> to provide more
             details.</p>'),
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
                    caption = "Preview",
                    tags$img(style = "width: 500px;",
                             src = "img/sps_wf.png",
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
