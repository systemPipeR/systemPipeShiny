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
                ), spsHr(),
                fluidRow(
                    h4(class = "text-center", "Save or load your snapshots"), br(),
                    #TODO transfer file to another project option
                    p("Snapshot files are encrypted. Files downloaded from this
                      page will not work on other SPS projects. Each SPS project has
                      its unique key. You should download and upload snapshot files
                      to the same SPS project.", class="text-orange text-center"),
                    column(3),
                    column(3, dynamicFile(id = ns("snap_upload"))),
                    column(3, strong("Download your snapshots"), br(),
                           tags$a(id = ns("save_snap"),
                                  class = "btn btn-default bttn-default shiny-download-link bttn-simple bttn-md",
                                  href = "", target = "_blank", download = NA,
                                  icon("file-download"), "Download")
                    )
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
        # trash snaps
        observeEvent(c(input$snap_btn, input$trash), ignoreInit = TRUE, {
            pushbar_open(id = ns("snap_push"))
            snaps <- names(shared$canvas$server)
            snap_remove <- which(snaps %in% input$destroy_order)
            if (length(snap_remove) > 0){
                snaps <- snaps[-snap_remove]
                shared$canvas$server <- shared$canvas$server[-snap_remove]
                shared$canvas$ui <- shared$canvas$ui[-snap_remove]
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
        # loading snap
        upload_path <- dynamicFileServer(input, session, id = "snap_upload")
        observeEvent(upload_path(), {
            confirmSweetAlert(
                session, inputId = "confirm_load_snap",
                title = "Load more snapshots?",
                text = "File uploaded, waiting for confirmation", type = "info"
            )
        })
        observeEvent(input$confirm_load_snap, ignoreNULL = TRUE, {
            req(isTRUE(input$confirm_load_snap))
            on.exit(snap_up$close())
            snap_up <- shiny::Progress$new()
            snap_up$set(0)
            snap_temp <- shinyCatch({
                snap_temp <- tempfile()
                snap_up$set(message = "decrypting")
                sps_enc$decrypt(upload_path()$datapath, snap_temp)
                snap_up$set(0.5)
                snap_up$set(message = "loading decrypted file")
                snap_temp <- readRDS(snap_temp)
                snap_up$set(0.8)
                snap_up$set(message = "validating object")
                if(!inherits(snap_temp, "list"))
                    stop("This is not a SPS snapshot file.")
                if(!all(names(snap_temp) %in% c("ui", "server")))
                    stop("Items in snapshot file should only have UI and Server")
                if(length(snap_temp$ui) == 0)
                    stop("Empty snapshot file")
                if(length(snap_temp$ui) != length(snap_temp$server))
                    stop("UI and Server in snapshot file doesn't have the same length")
                snap_up$set()
                snap_temp
            }, blocking_level = "error")
            shared$canvas <- snap_temp
        })
        # download snap
        observe({
            # if(not_empty(shared$canvas)) enable("save_snap")
            toggleState("save_snap", not_empty(shared$canvas$ui))
        })
        output$save_snap <- downloadHandler(
            filename = function(){
                glue('snap{Sys.time() %>% format("%Y%m%d%M%S")}.sps')
            },
            content = function(file) {
                req(not_empty(shared$canvas))
                on.exit(snap_down$close())
                snap_down <- shiny::Progress$new()
                snap_down$set(0)
                snap_down$set(message = "saving object")
                file_rds <- gsub(".sps", ".rds", file)
                saveRDS(shared$canvas, file_rds)
                snap_down$set(0.3)
                snap_down$set(message = "encrypting")
                sps_enc$encrypt(file_rds, file)
                snap_down$set(1)
            }
        )
    }
    moduleServer(id, module)
}

