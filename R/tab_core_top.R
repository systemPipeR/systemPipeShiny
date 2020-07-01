## UI
core_topUI <- function(id){
    ns <- NS(id)
    tagList(
        pushbar_deps(),
        actionBttn(
            inputId = ns("snap_btn"),
            label = "Snapshots",
            style = "bordered",
            size = "sm",
            icon = icon("camera")
        ),
        pushbar(
            id = ns("snap_push"),
            from = "top",
            style= "background:#ECF0F5;padding:2%;min-height:50%; overflow:auto;",
            tagList(
                fluidRow(
                    div(style = " position: fixed; top: 0; right: 0; margin:0;",
                        actionBttn(ns("close_snap"), style = "simple", icon = icon("times"),
                                   color = "danger", size = "sm")),
                    div(class = "text-center", tabTitle("Manage your plot snapshots")),
                    spsHr(),uiOutput(ns("top_snap"))
                )
            )
        )
    )
}

## server
core_topServer <- function(id, shared){
    module <- function(input, output, session){
        setup_pushbar(blur = TRUE, overlay = TRUE)
        ns <- session$ns
        observeEvent(c(input$snap_btn, input$trash), ignoreInit = TRUE, {
            pushbar_open(id = ns("snap_push"))
            snaps <- names(shared$canvas$server)
            snap_remove <- which(snaps %in% input$destroy_order)
            if (length(snap_remove) > 0){
                snaps <- snaps[-snap_remove]
                shared$canvas$server <- shared$canvas$server[-snap_remove]
                shared$canvas$ui <- shared$canvas$ui[-snap_remove]
                print(names(shared$canvas$server)); print(names(shared$canvas$ui))
            }
            output$top_snap <- renderUI({
                shiny::validate(need(length(isolate(shared$canvas$server)) > 0, message = "No snapshot yet"))
                tagList(
                    boxPlus(title = "Current snapshots", width = 6, closable = FALSE,
                            orderInput(ns("snaps"), NULL, items = snaps, placeholder = 'Current snapshots',
                                       item_class = "success", connect = c(ns("snaps"), ns('destroy')))
                    ),
                    boxPlus(title = "Snapshots to destroy", width = 6, closable = FALSE,
                            orderInput(ns('destroy'), NULL, items = NULL, placeholder = 'Drag plots you want to destroy here',
                                       connect = c(ns("snaps"), ns('destroy'))),
                            br(), br(), br(),
                            actionBttn(ns("trash"), "destroy", icon = icon("trash"),  style = "material-flat", color = "danger")
                    ),
                    tags$script(glue(.open = '@', .close = '@', '
                  $("#@ns("destroy")@").bind("DOMSubtreeModified", function(){
                    $(this).children(".btn").attr("class", "btn btn-danger ui-sortable-handle")
                  })
                ')),
                    tags$script(glue(.open = '@', .close = '@', '
                  $("#@ns("snaps")@").bind("DOMSubtreeModified", function(){
                    $(this).children(".btn").attr("class", "btn btn-primary ui-sortable-handle")
                  })
                '))
                )
            })
        })
        observeEvent(input$close_snap, {
            pushbar_close()
        })
    }
    moduleServer(id, module)
}

