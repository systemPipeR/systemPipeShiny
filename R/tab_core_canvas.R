
## UI
#' @importFrom colourpicker colourInput
#' @importFrom shinyWidgets sliderTextInput
#' @noRd
core_canvasUI <- function(id){
    ns <- NS(id)
    desc <- "
    ### About this tab
      This is a canvas. You can use it to compare different plots,
    combine different plots to be a figure panel, compare the same
    plot at different state, and so more...

    ### Use canvas
      To start with tab, you need to prepare a plot by clicking the
    `Render/Snapshot` button at any plot tab. The first time
    clicking that button is just rendering the plot for preview and you may
    adjust plotting options depending on what is available on that tab.
    From the second time of clicking the `Render` button, it will take a
    snapshot of that plot with the current state. You can take as
    many as you want.

      You can manage your snapshots by clicking the top banner of the app by the
    'camera' icon button. A dropdown menu will show up and you can
    see what snapshots you have at the moment. Drag snapshots to the right
    panel and click `Destory` will remove these snapshots. Clean unneed
    snapshots often. They consumes your RAM and it takes a lot longer to
    generate canvas if there are too many snapshots.

    "
    tagList(
        tabTitle("systemPipeShiny Canvas"),
        spsHr(), renderDesc(id = ns("desc"), desc), spsHr(),
        boxPlus(
            width = 12,
            collapsed = TRUE, closable = FALSE,
            h3("Manage snapshots", class="text-center"),
            p(
                "At least one snapshot required to display managing boxes",
                id = ns("snap_box_warn"),
                class = "text-orange"),
            uiOutput(ns("snap_box")),
            spsHr(), br(),
            #TODO transfer file to another project option
            p("Snapshot files are encrypted. Files downloaded from this
                      page will not work on other SPS projects. Each SPS
                      project has its unique key. You should download and
                      upload snapshot files to the same SPS project.",
              class="text-center"),
            column(3),
            column(3, dynamicFile(
                id = ns("snap_upload"), title = "Select snapshot file")),
            column(3, strong("Download your snapshots"), br(),
                   tags$a(id = ns("save_snap"),
                          class = "btn btn-default bttn-default
                                    shiny-download-link bttn-simple bttn-md",
                          href = "", target = "_blank", download = NA,
                          icon("file-download"), "Download")
            )
        ),
        boxPlus(
            title = "Canvas settings",
            width = 12,
            collapsible = TRUE, closable = FALSE,
            column(6,
                h5("Number of plots per row to initiate canvas:"),
                shinyWidgets::sliderTextInput(inputId = ns("ncols"),
                                label = NULL,
                                choices = c(1, 2, 3, 4, 12),
                                selected = 2,
                                grid = TRUE
                )
            ),
            column(6,
                h5("Choose canvas background color"),
                colourpicker::colourInput(
                    ns("canvas_color"),
                    label = NULL, "white",
                    allowTransparent = TRUE)
            )
        ),
        div(style = "text-align: center;",
            actionButton(ns("refresh"), 'Refresh Canvas')
        ), spsHr(),
        fluidRow(
            uiOutput(ns("canvas_main")),
            tags$script('stretchCanvas()')
        )
    )
}

## server
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyjqui orderInput
#' @noRd
core_canvasServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        # snapshot management
        observeEvent(c(shared$canvas$server, input$trash), {
            snaps <- names(shared$canvas$server)
            snap_remove <- which(snaps %in% input$destroy_order)
            if (length(snap_remove) > 0){
                snaps <- snaps[-snap_remove]
                shared$canvas$server <- shared$canvas$server[-snap_remove]
                shared$canvas$ui <- shared$canvas$ui[-snap_remove]
            }
            output$snap_box <- renderUI({
                shiny::validate(need(length(isolate(shared$canvas$server)) > 0,
                                     message = "No snapshot yet"))
                tagList(
                    shinydashboardPlus::boxPlus(
                        title = "Current snapshots",
                        width = 6, closable = FALSE,
                        shinyjqui::orderInput(
                            ns("snaps"),
                            style = "border-style: dotted; border-color: #b9b9b9; width:100%;",
                            NULL,
                            items = snaps,
                            placeholder = 'Current snapshots',
                            item_class = "success",
                            connect = c(ns("snaps"), ns('destroy'))
                        )
                    ),
                    shinydashboardPlus::boxPlus(
                        title = "Snapshots to destroy",
                        width = 6,
                        closable = FALSE,
                        shinyjqui::orderInput(
                            style = "border-style: dotted; border-color: #b9b9b9; width:100%;",
                            ns('destroy'),
                            NULL,
                            items = NULL,
                            placeholder = 'Drag plots you want to destroy here',
                            connect = c(ns("snaps"), ns('destroy'))
                        ),
                        br(),
                        shinyWidgets::actionBttn(
                            ns("trash"), "destroy",
                            icon = icon("trash"),
                            style = "material-flat",
                            color = "danger"
                        )
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
        # loading snap
        upload_path <- dynamicFileServer(input, session, id = "snap_upload")
        observeEvent(upload_path(), {
            shinyWidgets::confirmSweetAlert(
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
                    stop("Items in snapshot file should",
                         "only have UI and Server")
                if(length(snap_temp$ui) == 0)
                    stop("Empty snapshot file")
                if(length(snap_temp$ui) != length(snap_temp$server))
                    stop("UI and Server in snapshot",
                         "file doesn't have the same length")
                snap_up$set()
                snap_temp
            }, blocking_level = "error")
            shared$canvas <- snap_temp
        })
        # download snap
        observe({
            # if(not_empty(shared$canvas)) enable("save_snap")
            shinyjs::toggleState("save_snap", not_empty(shared$canvas$ui))
            shinyjs::toggleElement(
                id = "snap_box_warn", anim = TRUE,
                condition = !not_empty(shared$canvas$ui)
            )
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
        # receive snap singal
        observeEvent(shared$snap_signal, {
            tab_id <- shared$snap_signal[1]
            new_plot_id <- glue("{tab_id}-{shared$snap_signal[2]}")
            shared$canvas$ui[[new_plot_id]] <-
                list(sps_plots$getUI(tab_id, ns(new_plot_id)), ns(new_plot_id))
            shared$canvas$server[[new_plot_id]] <- sps_plots$getServer(tab_id)
        })
        # refresh canvas on click
        observeEvent(input$refresh, {
            ui <- shared$canvas$ui
            snap_severs <- shared$canvas$server
            snaps <- which(names(ui) %in% input$snaps_order)
            # create plot containers
            output$canvas_main <- renderUI({
                shiny::validate(need(length(snaps) > 0,
                                     message = "Need at least one snapshot"))
                ui <- ui[snaps]
                snap_severs <- snap_severs[snaps]
                lapply(seq_along(ui), function(i){
                    column(
                        width = 12/isolate(input$ncols),
                        classspsPlugin.R = "collapse in",
                        id = glue("{ui[[i]][2]}-container"),
                        div(class = "snap-drag bg-primary",
                            h4(glue("Plot {ui[[i]][2]}")),
                            tags$button(
                                class = "btn action-button canvas-close",
                                icon("times"),
                                `data-toggle`="collapse",
                                `data-target`=glue("#{ui[[i]][2]}-container"),
                                `plot-toggle` = ui[[i]][2]
                            )
                        ),
                        div(class = "sps-plot-canvas", ui[[i]][[1]]),
                        tags$script(glue(.open = '@', .close = '@',
                                         '$("#@ui[[i]][2]@-container")',
                                         '.draggable({ handle: ".snap-drag"})')
                                    ),
                        tags$script(glue(.open = '@', .close = '@',
                                         '$("#@ui[[i]][2]@")',
                                         '.resizable()'))
                    )
                }) %>%{
                    fluidRow(id = ns('plots'),
                             style = glue("background-color: {isolate(input$canvas_color)};"),
                             class = "sps-canvas",
                             tagList(.)
                    )
                }
            })
            # add server to plots
            for(plot_id in names(snap_severs)){
                output[[plot_id]] <- snap_severs[[plot_id]]
            }
        })
    }
    moduleServer(id, module)
}


