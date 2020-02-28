## UI
topUI <- function(id){
    ns <- NS(id)
    tagList(
        pushbar_deps(),
        actionBttn(
            inputId = ns("top_target_btn"),
            label = "Targets",
            style = "bordered", 
            size = "sm",
            icon = icon("vials")
        ),
        pushbar(
            id = ns("top_target_push"),
            from = "top",
            style= "background:#ECF0F5;padding:2%;min-height:50%;",
            tagList(
                fluidRow(
                    actionBttn(ns("close_target_push"), style = "simple", icon = icon("times"), color = "danger", size = "sm")
                    ),
                fluidRow(
                    boxPlus(title = "Targets", width = 12, closable = FALSE,
                            p("You need to load your targets first at the 'Targets' tab, otherwise nothing will show up here."),
                            rHandsontableOutput(ns("targets_df"))
                            )
                )
            )
        ),
        actionBttn(
            inputId =  ns("top_ct_btn"),
            label = "Count",
            style = "bordered",
            size = "sm",
            icon = icon("table")
        ),
        pushbar(
            id = ns("top_ct_push"),
            from = "top",
            style= "background:#ECF0F5;padding:2%;min-height:50%;",
            tagList(
                fluidRow(
                    actionBttn(ns("close_ct_push"), style = "simple", icon = icon("times"), color = "danger", size = "sm")
                ),
                fluidRow(
                    boxPlus(title = "Count Table", width = 12, closable = FALSE,
                            p("You need to load your targets first at the 'DEG' tab, otherwise nothing will show up here."),
                            rHandsontableOutput(ns("count_table"))
                    )
                )
            )
        ),
        actionBttn(
            inputId = ns("top_bed_btn"),
            label = "BED",
            style = "bordered",
            size = "sm",
            icon = icon("dna")
        ),
        pushbar(
            id = ns("top_bed_push"),
            from = "top",
            style= "background:#ECF0F5;padding:2%;min-height:50%;",
            tagList(
                fluidRow(
                    actionBttn(ns("close_bed_push"), style = "simple", icon = icon("times"), color = "danger", size = "sm")
                ),
                fluidRow(
                    boxPlus(title = "BED", width = 12, closable = FALSE,
                            p("You need to load your targets first at the 'xx' tab, otherwise nothing will show up here."),
                            rHandsontableOutput(ns("bed"))
                    )
                )
            )
        )
    )
}

## server
topServer <- function(input, output, session, shared){
    setup_pushbar(blur = TRUE, overlay = TRUE)
    ns <- session$ns
    observeEvent(input$top_target_btn, {
        pushbar_open(id = ns("top_target_push"))
        if (!is.null(shared$targets$df)){
            output$targets_df <- renderRHandsontable({
                rhandsontable(shared$targets$df, readOnly = TRUE) %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            })
        }
    })
    observeEvent(input$top_ct_btn, {
        pushbar_open(id = ns("top_ct_push"))
        if (!is.null(shared$count_table)){
            output$count_table <- renderRHandsontable({
                rhandsontable(shared$count_table, readOnly = TRUE) %>%
                    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            })
            }
        })
    observeEvent(input$top_bed_btn, {
        pushbar_open(id = ns("top_bed_push"))
        if (!is.null(shared$bed)){
            output$bed <- renderRHandsontable({
                rhandsontable(shared$bed, readOnly = TRUE) %>%
                    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            })
        }
    })
    observeEvent({input$close_target_push | input$close_ct_push | input$close_bed_push}, {
        pushbar_close()
    }) 
}
