## UI
#' @importFrom shinydashboardPlus widgetUserBox
#' @noRd
core_aboutUI <- function(id){
    ns <- NS(id)
    tags$div(
        tabTitle("About this app"),
        h3("Our group"),
        fluidRow(
            shinydashboardPlus::widgetUserBox(
                title = "Le Zhang",
                subtitle = "Student",
                type = NULL,
                width = 4,
                height = 500,
                src = "https://avatars0.githubusercontent.com/u/35240440?s=460&v=4",
                closable = FALSE,
                collapsible = FALSE,
                footer =  HTML('<a href="mailto:le.zhang001@email.ucr.edu">&lt;email Le Zhang&gt;</a>'),
                includeMarkdown("www/about/lzhang.md")
            ),
            shinydashboardPlus::widgetUserBox(
                title = "Ponmathi Ramasamy",
                subtitle = "Student",
                type = NULL,
                width = 4,
                height = 500,
                src = "https://avatars2.githubusercontent.com/u/45085174?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE,
                footer = HTML('<a href="mailto:prama008@ucr.edu">&lt;email Ponmathi Ramasamy&gt;</a>'),
                includeMarkdown("www/about/pramasamy.md")
            ),
            shinydashboardPlus::widgetUserBox(
                title = "Daniela Cassol",
                subtitle = "Postdoc",
                type = NULL,
                width = 4,
                height = 500,
                src = "https://avatars2.githubusercontent.com/u/12722576?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE,
                footer = HTML('<a href="mailto:danielac@ucr.edu">&lt;email Daniela Cassol&gt;</a>'),
                includeMarkdown("www/about/dcassol.md")
            ),
            shinydashboardPlus::widgetUserBox(
                title = "Jianhai Zhang",
                subtitle = "Student",
                type = NULL,
                width = 4,
                height = 500,
                src = "https://avatars0.githubusercontent.com/u/22919387?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE,
                footer = HTML('<a href="mailto:jzhan067@ucr.edu">&lt;email Jianhai Zhang&gt;</a>'),
                includeMarkdown("www/about/jzhang.md")
            ),
            shinydashboardPlus::widgetUserBox(
                title = "Gordon Mosher",
                subtitle = "Student",
                type = NULL,
                width = 4,
                height = 500,
                src = "https://avatars0.githubusercontent.com/u/8660309?s=460&v=4",
                closable = FALSE,
                collapsible = FALSE,
                footer = HTML('<a href="mailto:gmosh001@ucr.edu ">&lt;email Gordon Mosher&gt;</a>'),
                includeMarkdown("www/about/gmosher.md")
            ),
            shinydashboardPlus::widgetUserBox(
                title = "Thomas Girke",
                subtitle = "PI",
                type = NULL,
                width = 4, height = 500,
                src = "https://avatars3.githubusercontent.com/u/1336916?s=400&v=4",
                closable = FALSE,
                collapsible = FALSE,
                footer = HTML('<a href="mailto:tgirke@ucr.edu">&lt;email Thomas Girke&gt;</a>'),
                includeMarkdown("www/about/tgirke.md")
            )
        ),
        h3("Read the manual"),
        tags$iframe(src="https://systempiper.github.io/systemPipeShiny-book/",
                    style="border: 1px solid #AAA; width: 100%; height: 700px"),
        br(),
        tags$a(href="https://bioconductor.org/packages/release/bioc/html/systemPipeR.html",
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
