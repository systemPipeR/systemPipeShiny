
## UI
#' @importFrom colourpicker colourInput
#' @importFrom shinyWidgets sliderTextInput
core_canvasUI <- function(id){
    ns <- NS(id)
    desc <- "
    ### About this tab
      This is a canvas. You can use it to compare different plots, combine different
    plots to be a figure panel, compare the same plot at different state, and
    so much more...

    ### Use canvas
      To start with tab, you need to prepare a plot by clicking the `Render/Snapshot`
    button at any plot tab. The first time clicking that button is just rendering
    the plot for preview and you may adjust plotting options depending on what
    is available on that tab. From the second time of clicking the `Render` button,
    it will take a snapshot of that plot with the current state. You can take as
    many as you want.

      You can manage your snapshots by clicking the top banner of the app by the
    'camera' icon button. A dropdown menu will show up and you can see what snapshots
    you have at the moment. Drag snapshots to the right panel and click `Destory`
    will remove these snapshots. Clean unneed snapshots often. They consumes your
    RAM and it takes a lot longer to generate canvas if there are too many snapshots.

    "
    tagList(
        tabTitle("systemPipeShiny Canvas"),
        spsHr(), renderDesc(id = ns("desc"), desc), spsHr(),
        fluidRow(
            column(6,
                h5("Number of plots per row to initiate canvas:"),
                shinyWidgets::sliderTextInput(inputId = ns("ncols"),
                                label = NULL,
                                choices = c(1:4, 12),
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
        fluidRow(uiOutput(ns("snap_choose"))),
        fluidRow(
            uiOutput(ns("canvas_main")),
            tags$script('stretchCanvas()')
        )
    )
}

## server
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyjqui orderInput
core_canvasServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        # receive snap singal
        observeEvent(shared$snap_signal, {
            tab_id <- shared$snap_signal[1]
            new_plot_id <- glue("{tab_id}-{shared$snap_signal[2]}")
            shared$canvas$ui[[new_plot_id]] <- list(sps_plots$getUI(tab_id, ns(new_plot_id)), ns(new_plot_id))
            shared$canvas$server[[new_plot_id]] <- sps_plots$getServer(tab_id)
        })
        # update snap list on signal
        observeEvent(shared$canvas$server, {
            output$snap_choose <- renderUI({
                shiny::validate(need(length(shared$canvas$server) > 0, message = "No snapshot yet"))
                tagList(
                    shinydashboardPlus::boxPlus(title = "Current snapshots", width = 6, closable = FALSE,
                            shinyjqui::orderInput(
                                ns("snaps"), NULL,
                                items = names(shared$canvas$server),
                                placeholder = 'Current snapshots',
                                item_class = "success",
                                connect = c(ns("snaps"), ns('snap_unselect'))
                            )
                    ),
                    shinydashboardPlus::boxPlus(title = "Snapshots excluded", width = 6, closable = FALSE,
                            shinyjqui::orderInput(
                                ns('snap_unselect'), NULL, items = NULL,
                                placeholder = 'Drag plots here if you don\'t want to see on canvas',
                                connect = c(ns("snaps"), ns('snap_unselect'))
                            )
                    ),
                    tags$script(glue(.open = '@', .close = '@', '
                  $("#@ns("snap_unselect")@").bind("DOMSubtreeModified", function(){
                    $(this).children(".btn").attr("class", "btn btn-danger ui-sortable-handle")
                  })
                ')),
                    tags$script(glue(.open = '@', .close = '@', '
                  $("#@ns("snaps")@").bind("DOMSubtreeModified", function(){
                    $(this).children(".btn").attr("class", "btn btn-success ui-sortable-handle")
                  })
                '))
                )
            })
        })
        # refresh canvas on click
        observeEvent(input$refresh, {
            ui <- shared$canvas$ui
            snap_severs <- shared$canvas$server
            snaps <- which(names(ui) %in% input$snaps_order)
            # create plot containers
            output$canvas_main <- renderUI({
                shiny::validate(need(length(snaps) > 0, message = "Need at least one snapshot"))
                ui <- ui[snaps]
                snap_severs <- snap_severs[snaps]
                sapply(seq_along(ui), function(i){
                    column(
                        width = 12/isolate(input$ncols),
                        class = "collapse in",
                        id = glue("{ui[[i]][2]}-container"),
                        div(class = "snap-drag bg-primary",
                            h4(glue("Plot {ui[[i]][2]}")),
                            tags$button(
                                class = "btn action-button canvas-close", icon("times"),
                                `data-toggle`="collapse",
                                `data-target`=glue("#{ui[[i]][2]}-container"),
                                `plot-toggle` = ui[[i]][2]
                            )
                        ),
                        div(class = "sps-plot-canvas", ui[[i]][[1]]),
                        tags$script(glue(.open = '@', .close = '@',
                                         '$("#@ui[[i]][2]@-container")',
                                         '.draggable({ handle: ".snap-drag"})')),
                        tags$script(glue(.open = '@', .close = '@',
                                         '$("#@ui[[i]][2]@")',
                                         '.resizable()'))
                    )
                }, simplify = FALSE) %>%{
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


