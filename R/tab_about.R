## UI
aboutUI <- function(id){
    ns <- NS(id)
    tags$div(
        h2("About this app", style = "color:var(--info)"),
        h3("Our group"),
        fluidRow(
            widgetUserBox(
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
            widgetUserBox(
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
            widgetUserBox(
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
            widgetUserBox(
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
            widgetUserBox(
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
            widgetUserBox(
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
        h3("About SystemPipeR"),
        tags$iframe(src="https://girke.bioinformatics.ucr.edu/systemPipeR/mydoc_systemPipeR_2.html",
                    style="border: 1px solid #AAA; width: 100%; height: 700px"),
        br(),
        tags$a(href="https://bioconductor.org/packages/release/bioc/html/systemPipeR.html",
               "Visist Bioconductor page")
    )

}

## server
aboutServer <- function(input, output, session){


}
