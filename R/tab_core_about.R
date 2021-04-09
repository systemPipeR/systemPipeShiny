## UI
#' @importFrom shinydashboardPlus userBox
#' @noRd
core_aboutUI <- function(id){
    ns <- NS(id)
    tags$div(
        tabTitle("About this app"),
        box(
            closable = FALSE,
            collapsible = TRUE,
            width = 12,
            style = "height: 500px; overflow-Y: auto;",
            if(checkUrl("https://raw.githubusercontent.com/lz100/systemPipeShiny/master/NEWS.md")){
                includeMarkdown("https://raw.githubusercontent.com/lz100/systemPipeShiny/master/NEWS.md")
            } else {
                div(
                    h3("Cannot load SPS updates"),
                    p("Visit here:"),
                    tags$a(href="https://github.com/systemPipeR/systemPipeShiny/blob/master/NEWS.md",
                           target="_blank",
                           "https://github.com/systemPipeR/systemPipeShiny/blob/master/NEWS.md")
                )
            }
        ), spsHr(),
        h3("Developers"),

        fluidRow(
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/lz100">Le Zhang</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars0.githubusercontent.com/u/35240440?s=460&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/dcassol">Daniela Cassol</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars2.githubusercontent.com/u/12722576?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Postdoc'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/mathrj">Ponmathi Ramasamy</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars2.githubusercontent.com/u/45085174?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/jianhaizhang">Jianhai Zhang</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars0.githubusercontent.com/u/22919387?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/gdmosher">Gordon Mosher</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars0.githubusercontent.com/u/8660309?s=460&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://girke.bioinformatics.ucr.edu">Thomas Girke</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars3.githubusercontent.com/u/1336916?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'PI'
            ),
        ), spsHr(),
        markdown('### [Read the manual{blk}](https://systempipe.org/sps/)'),
        br()
    )
}

## server
core_aboutServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}
