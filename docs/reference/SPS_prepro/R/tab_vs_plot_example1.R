## UI


plot_example1UI <- function(id){
    ns <- NS(id)
    desc <- "
    #### An exmaple plot tab to make a PCA plot from RPKM
    This tab use R builtin functions to make a very simple PCA from if data is
    prepared from 'Data tab example'.
    "
    tagList(
        pgPaneUI(ns("pg"),
                 titles = c("Package Requirements",
                            "Input Dataframe",
                            "Validate Dataframe"),
                 pg_ids = c(ns("pkg"),
                            ns("data"),
                            ns("vd_data"))
        ),
        tabTitle("PCA Plot"),
        spsHr(), h3("Descrption"),
        hexPanel(ns("poweredby"), "POWERED BY:",
                 hex_imgs = c("img/sps.png"),
                 hex_titles = c("SystemPipeShiny"), ys = c("-10")),
        renderDesc(id = ns("desc"), desc),
        spsHr(), h3("Data preparation"),
        fluidRow(
            column(12,
                   genHrefTab(
                       c("data_example"),
                       title = "You need have the count table from these tabs:"))
        ),
        h5("Once you have prepared the data,
           select which tab(s) your data is coming from:"),
        column(12, shinyWidgets::pickerInput(ns("source_data"), "Count Data",
                    choices = c("Count table" = "data_example"),
                    options = list(style = "btn-primary"))), spsHr(),
        div(style = "text-align: center;",
            strong("Click the button below to start or reload data"), br(),
            actionButton(inputId = ns("validate_start"), label = "Start/Reload")
        ),
        spsHr(), h3("Plotting"),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            shinydashboardPlus::boxPlus(width = 12,
                title = "Some exmaple options to control the plot",
                closable = FALSE,
                clearableTextInput(ns("plot_title"),
                                   label = "Plot title",
                                   value = "Count Table PCA"),
                shinyWidgets::pickerInput(
                    ns("point_size"), "Point Size",
                    choices = c(1, 2, 3, 4, 5),
                    options = list(style = "btn-primary")
                )
            ),
            spsHr(),
            fluidRow(
                actionButton(ns("render"), label = "Render/Snapshot plot",
                             icon("paper-plane")),
            ),
            div(class = "sps-plot-container",
                shinyjqui::jqui_resizable(
                    sps_plots$addUI(plotly::plotlyOutput(ns("plot")), id)
                ),
                tags$script(glue('stretchPlotTab("{ns("plot")}")'))
            )
        )
    )
}

## server
plot_example1Server <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "plot_example1"
        # define data containers
        mydata <- reactiveValues()
        # start the tab by checking if required packages are installed
        observeEvent(input$validate_start, {
            req(shinyCheckPkg(session = session,
                                cran_pkg = c("base"),
                                bioc_pkg = c(""),
                                github = c("")
            ))
            pgPaneUpdate('pg', 'pkg', 100)
            mydata$data <- getData(isolate(input$source_data), shared)
            pgPaneUpdate('pg', 'data', 100)
            spsValidate({
                if (ncol(mydata$data) > 1) TRUE
                else stop("Need more than 1 column")
            }, "Raw data column check")

            pgPaneUpdate('pg', 'vd_data', 100)
            shinyjs::show(id = "tab_main")
            shinytoastr::toastr_success(
                title = "Ready for plotting!", message = "", timeOut = 5000,
                position = "bottom-right"
            )
        })
        observeEvent(input$render, {
            output$plot <- sps_plots$addServer(plotly::renderPlotly, tab_id, {
                samples <- rownames(mydata$data)
                plotly::ggplotly(
                   ggplot(mydata$data %>% as.data.frame()) +
                       geom_point(aes(x= PC1, y = PC2, color = samples),
                                  size = as.numeric(input$point_size)) +
                       ggtitle(as.character(input$plot_title))
                )
            })
            shared$snap_signal <- sps_plots$notifySnap(tab_id)
            req(shared$snap_signal)
            shinytoastr::toastr_info(
                glue("Snapshot {glue_collapse(shared$snap_signal, '-')}",
                     "added to canvas"),
                position = "bottom-right")
        })
    }
    moduleServer(id, module)
}
