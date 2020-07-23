## UI
#' @importFrom shinydashboardPlus carousel carouselItem
#' @noRd
core_dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("systemPipeShiny Welcome"),
        h2(strong("This app is under construction,
                  please do not use it for production.")),
        tags$p(strong("This page controls targets file and
                      configuration of other parameters.")),
        p("If you cannot click some buttons, that means they are
          disabled at current tab or you need to do other things first,
          e.g. upload a file."),
        fluidRow(
            shinydashboardPlus::carousel(width = 12,
                id = "dashcarousel",
                shinydashboardPlus::carouselItem(
                    caption = "SPS",
                    tags$img(style = "width: 500px;",
                             src = "img/systemPipeR_site.png",
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
