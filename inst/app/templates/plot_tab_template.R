######################SPS #@tab_displayname@# tab######################
## creation date: #@crt_date@#
## Author: #@author@#

## lines with `#####` around are important sections, you may change/add  the
## values inside if you used default settings when creating the tab.

## UI for #@tab_id@#

#@tab_id@#UI <- function(id){
    ns <- NS(id)
    desc <- "
    #@{desc}@#
    "
    tagList(
        ##### Progress tracker
        pgPaneUI(ns("pg"),
                 titles = c("Package Requirements",
                            #@pg_title@#),
                 pg_ids = c(ns("pkg"),
                            #@pg_id@#)
        ),
        #####
        tabTitle("#@tab_displayname@#"),
        spsHr(), h3("Descrption"),
        hexPanel(ns("poweredby"), "POWERED BY:",
                 hex_imgs = c("img/logo.png"),
                 hex_titles = c("SystemPipeShiny"), ys = c("-10")),
        renderDesc(id = ns("desc"), desc),
        spsHr(), h3("Data preparation"),
        fluidRow(
            #@hreftab@#
        ),
        h5("Once you have prepared the data,
           select which tab(s) your data is coming from:"),
        ##### select input data
        fluidRow(
            #@select_input@#
        ),
        spsHr(),
        #####
        div(style = "text-align: center;",
            strong("Click the button below to start or reload data"), br(),
            actionButton(inputId = ns("validate_start"), label = "Start/Reload")
        ),
        spsHr(), h3("Plotting"),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            ##### UI of different plot options
            div(
                id = ns("plot_opts"), style = "background: white;",
                #@control_ui@#
                ),
            #####
            spsHr(),
            fluidRow(
                actionButton(ns("render"), label = "Render/Snapshot plot",
                             icon("paper-plane")),
            ),
            div(class = "sps-plot-container",
                shinyjqui::jqui_resizable(
                    ##### plot container
                    sps_plots$addUI(#@p_out_func@#(ns("plot")), id)
                    #####
                ),
                tags$script(glue::glue('stretchPlotTab("{ns("plot")}")'))
            )
        )
    )
}

## server for #@tab_id@#

#@tab_id@#Server <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "#@tab_id@#"
        # define a data container for all data sets
        mydata <- reactiveValues()
        observeEvent(input$validate_start, {
            ##### package check
            req(shinyCheckPkg(session = session,
                              #@pkgs@#
            ))
            #####
            pgPaneUpdate('pg', 'pkg', 100)
            ##### get and validate input data sets
            #@getdata@#
            #@vd@#
            #####
            shinyjs::show(id = "tab_main")
            shinytoastr::toastr_success(
                title = "Ready for plotting!", message = "", timeOut = 5000,
                position = "bottom-right"
            )
        })
        observeEvent(input$render, {
            ##### plotting function
            output$plot <- sps_plots$addServer(#@p_render_func@#, tab_id, {
                #@plot_expr@#
            })
            #####
            shared$snap_signal <- sps_plots$notifySnap(tab_id)
            req(shared$snap_signal)
            shinytoastr::toastr_info(
                glue::glue("Snapshot {glue::glue_collapse(shared$snap_signal, '-')}",
                     "added to canvas"),
                position = "bottom-right")
        })
    }
    moduleServer(id, module)
}
