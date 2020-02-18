## UI
aboutUI <- function(id){
    ns <- NS(id)
    tagList(
        h2("About this app"),
        tags$p("Designed by xxx, xxx"),
        h3("Support"),
        fluidRow(
            widgetUserBox(
                title = "Thomas Girke",
                subtitle = "PI",
                type = NULL,
                width = 6,
                src = "https://avatars3.githubusercontent.com/u/1336916?s=400&v=4",
                background = TRUE,
                backgroundUrl = "https://bashooka.com/wp-content/uploads/2018/04/scg-canvas-background-animation-24.jpg",
                closable = FALSE,
                collapsible = FALSE,
                HTML('Thomas Girke <a href="mailto:tgirke@ucr.edu">&lt;tgirke@ucr.edu&gt;</a>')
            ),
            widgetUserBox(
                title = "Le Zhang",
                subtitle = "Student",
                type = NULL,
                width = 6,
                src = "https://avatars0.githubusercontent.com/u/35240440?s=460&v=4",
                background = TRUE,
                backgroundUrl = "https://bashooka.com/wp-content/uploads/2018/04/scg-canvas-background-animation-24.jpg",
                closable = FALSE,
                collapsible = FALSE,
                HTML('Le Zhang <a href="mailto:le.zhang001@email.ucr.edu">&lt;le.zhang001@email.ucr.edu&gt;</a>')
            )
        ),
        fluidRow(
            widgetUserBox(
                title = "Ponmathi Ramasamy",
                subtitle = "Student",
                type = NULL,
                width = 6,
                src = "https://avatars2.githubusercontent.com/u/45085174?s=400&v=4",
                background = TRUE,
                backgroundUrl = "https://bashooka.com/wp-content/uploads/2018/04/scg-canvas-background-animation-24.jpg",
                closable = FALSE,
                collapsible = FALSE,
                HTML('Ponmathi Ramasamy <a href="mailto:prama008@ucr.edu">&lt;prama008@ucr.edu&gt;</a>')
            ),
            widgetUserBox(
                title = "Daniela Cassol",
                subtitle = "Postdoc",
                type = NULL,
                width = 6,
                src = "https://avatars2.githubusercontent.com/u/12722576?s=400&v=4",
                background = TRUE,
                backgroundUrl = "https://bashooka.com/wp-content/uploads/2018/04/scg-canvas-background-animation-24.jpg",
                closable = FALSE,
                collapsible = FALSE,
                HTML('Daniela Cassol <a href="mailto:danielac@ucr.edu">&lt;danielac@ucr.edu&gt;</a>')
            )
        ),
        h3("About SystemPipeR"),
        tags$iframe(src="http://girke.bioinformatics.ucr.edu/systemPipeR/mydoc_systemPipeR_2.html",
                    style="border: 1px solid #AAA; width: 100%; height: 700px"),
        br(),
        tags$a(href="https://bioconductor.org/packages/release/bioc/html/systemPipeR.html",
               "Visist Bioconductor page")
    )
    
}

## server
aboutServer <- function(input, output, session){
    
}