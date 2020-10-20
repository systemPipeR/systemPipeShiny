## UI
#' @importFrom shinydashboardPlus widgetUserBox
#' @noRd
core_aboutUI <- function(id){
    ns <- NS(id)
    tags$div(
        tabTitle("About this app"),
        fluidRow(
            if(checkUrl("https://raw.githubusercontent.com/lz100/systemPipeShiny/master/NEWS.md")){
                includeMarkdown("https://raw.githubusercontent.com/lz100/systemPipeShiny/master/NEWS.md")
            } else {
                div(
                    h3("Cannot get SPS updates"),
                    p("Visit here:"),
                    tags$a(href="https://github.com/systemPipeR/systemPipeShiny/blob/master/NEWS.md",
                           "https://github.com/systemPipeR/systemPipeShiny/blob/master/NEWS.md")
                )
            }
        ), spsHr(),
        h3("Developers"),
        fluidRow(
            shinydashboardPlus::widgetUserBox(
                title = HTML('<a href="mailto:le.zhang001@email.ucr.edu">Le Zhang</a>'),
                subtitle = "Student",
                type = 2,
                width = 3,
                src = "https://avatars0.githubusercontent.com/u/35240440?s=460&v=4",
                closable = FALSE,
                collapsible = FALSE
            ),
            shinydashboardPlus::widgetUserBox(
                title = HTML('<a href="mailto:danielac@ucr.edu">Daniela Cassol</a>'),
                subtitle = "Postdoc",
                type = 2,
                width = 3,
                src = "https://avatars2.githubusercontent.com/u/12722576?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE
            ),
            shinydashboardPlus::widgetUserBox(
                title =  HTML('<a href="mailto:prama008@ucr.edu">Ponmathi Ramasamy</a>'),
                subtitle = "Student",
                width = 3,
                type = 2,
                src = "https://avatars2.githubusercontent.com/u/45085174?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE
            ),
            shinydashboardPlus::widgetUserBox(
                title = HTML('<a href="mailto:jzhan067@ucr.edu">Jianhai Zhang</a>'),
                subtitle = "Student",
                type = 2,
                width = 3,
                src = "https://avatars0.githubusercontent.com/u/22919387?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE
            ),
            shinydashboardPlus::widgetUserBox(
                title = HTML('<a href="mailto:gmosh001@ucr.edu ">Gordon Mosher</a>'),
                subtitle = "Student",
                type = 2,
                width = 3,
                src = "https://avatars0.githubusercontent.com/u/8660309?s=460&v=4",
                closable = FALSE,
                collapsible = FALSE
            ),
            shinydashboardPlus::widgetUserBox(
                title = HTML('<a href="mailto:tgirke@ucr.edu">Thomas Girke</a>'),
                subtitle = "PI",
                type = 2,
                width = 3,
                src = "https://avatars3.githubusercontent.com/u/1336916?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE
            )
        ), spsHr(),
        h3("Read the manual"),
        tags$iframe(src="https://systempiper.github.io/systemPipeShiny-book/",
                    style="border: 1px solid #AAA; width: 100%; height: 700px"),
        br(), spsHr(),
        tags$a(href="https://bioconductor.org/packages/release/bioc/html/systemPipeShiny.html",
               "Visist Bioconductor page")
    )

}

## server
core_aboutServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}
