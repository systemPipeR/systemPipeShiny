timelineItem(
    id = "timeline",
    title = "this title",
    icon = icon("times"),
    color = "red",
    border = FALSE,
    progressBar(
        "pg",  striped = TRUE, status = "primary",
        100)
)
string = c("a", "b")
main_title = "Plot Prepare"
title = c("this a", "this b")
value = c(1, 2)
id = "thispg"
test = sapply(seq_along(string), function(i) {
    tags$li(style = "margin-bottom: 0;",
        tags$i(id = glue("{string[i]}-icon"), class = "fa fa-times bg-red"),
        div(class = "timeline-item",
            h3(class = "timeline-header no-border", title[i]),
            div(class="timeline-body", style = "padding: 0px;",
                progressBar(
                    glue("pg-{string[i]}"), striped = TRUE,
                    status = "primary", value[i]
                )
            )
        )
    )
}, simplify = FALSE) %>% {
    timelineBlock(reversed = FALSE, id = glue("{id}-timeline"),
                  .,
                  timelineLabel(id = glue("{id}-pg-label"), "Ready",
                                color = "orange"),
                  div(style = "margin-left: 60px; margin-right: 15px;",
                      progressBar(
                          glue("{id}-pg-all"), striped = TRUE,
                          status = "primary", 0
                      )
                  )
    )
} %>% {
    div(class = "tab-pane", id = glue("{id}-pg-container"),
        absolutePanel(
            top = "5%", right = "2%", draggable = TRUE, width = "300",
            height = "auto", class = "control-panel", cursor = "inherit",
            style = "background-color: white; z-index:999;",
            fluidRow(
                column(3),
                column(7, h4(main_title)),
                column(2,
                       HTML(glue('<button class="action-button ',
                                 'bttn bttn-simple bttn-xs bttn-primary ',
                                 'bttn-no-outline" data-target="#{id}-pg-collapse"',
                                 ' data-toggle="collapse">',
                                 '<i class="fa fa-minus"></i></button>')))
            ),
            div(class = "collapse", id = glue("{id}-pg-collapse"), .)
        )
    )
}

library(shiny)
library(shinydashboard)
library(shinytoastr)
ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = dashboardSidebar(),
                    body = dashboardBody(
                        useSps(),
                        tags$head(tags$script(src="sps/sps_update_pg.js"),
                             tags$script(src="sps/ElementQueries.js")),
                        useToastr(),
                        actionButton("a", "a"),
                        actionButton("b", "b"),
                        actionButton("c", "c"),
                        test
                    )
)
server <- function(input, output, session) {
    ns <- NS("test")
    pg_values <- reactiveValues()
    output$pg <- pgPaneServer(c("This a" = "a",
                                "This b" = "b",
                                "This c" = "c"),
                              pg_values, ns)
    observeEvent(input$a, {
        for(i in 1:10){
            updateProgressBar(session, id = "pg-a", value = i*10)
            session$sendCustomMessage(
                type = "sps-update-pg",
                message = list(
                    panel_id = id,
                    which_pg = "a",
                    value = i*10
                ))
            Sys.sleep(0.3)
        }


    })
    observeEvent(input$b, {
        for(i in 1:10){
            updateProgressBar(session, id = "pg-b", value = i*10)
            session$sendCustomMessage(
                type = "sps-update-pg",
                message = list(
                    panel_id = id,
                    which_pg = "b",
                    value = i*10
                ))
            Sys.sleep(0.3)
        }
    })
}
shinyApp(ui, server)
