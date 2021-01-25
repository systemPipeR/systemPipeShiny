###################### SPS Plot Tab Title tab######################
## creation date: 2021-01-21 15:42:15
## Author:

## lines with `#####` around are important sections, you may change/add  the
## values inside if you used default settings when creating the tab.

## UI for vs_esquisse
#' @importFrom esquisse esquisseContainer esquisserUI
vs_esqUI <- function(id) {
    ns <- NS(id)
    desc <- "
    #### Make plots in one second

    This module enables you to quickly upload datasets and make a {[ggplot](https://ggplot2.tidyverse.org/)}
    in a second by using some functionalities from  {[Esquisse](https://dreamrs.github.io/esquisse/index.html)}.

    1. If you don't have an analysis-ready dataset, simply **confirm** and use the example
       dataset and follow the [guide](https://dreamrs.github.io/esquisse/articles/get-started.html).
       For most UI parts, you don't need to use the guide, they should be very easy to understand.
    2. Upload your own dataset, choose the right delimiter and comment symbols to parse
       the data file. e.g., a typical `csv` file use `,` as delimiter and `#` to mark
       comment lines. Confirm this new dataset and the plot maker should be updated.
    3. Interact with `SPS Canvas`. When the plot is displayed, you can click the
       **To Canvas** button to send a screenshot of current plot and make some
       other precise image editing on the `Canvas` tab.
    4. SPS is not a professional data editor, so editing the data table is disabled
       on this module. You may want to preprocess it before uploading.

    "
    tagList(
        tags$head(
            tags$script(src="sps/js/sps_esq.js"),
            tags$link(rel="stylesheet", href = "sps/css/sps_esq.css")
        ),
        tabTitle("Quick {ggplot}"),
        renderDesc(id = ns("desc"), desc),
        ## Progress
        h3("Confirm dataset for quick plots", class = "text-center text-info"),
        spsTimeline(
            ns("df_status"),
            up_labels = c("1", "2"),
            down_labels = c("Dataset", "Data Ready"),
            icons = list(
                icon("table"),
                icon("check")
            ),
            completes = c(TRUE, FALSE)
        ),
        bsplus::bs_accordion(id = ns("main_panel")) %>%
            bsplus::bs_set_opts(panel_type = "info") %>%
            bsplus::bs_append(
                title = "1. Confirm to use example table or upload a new one",
                fluidRow(
                    h3("Load table"),
                    column(
                        3,
                        boxPlus(
                            closable = FALSE, width = 12,
                            radioGroupButtons(
                                inputId = ns("source_df"),
                                label = "Choose your file source:",
                                selected = "eg",
                                choiceNames = c("Upload", "Example"),
                                choiceValues = c("upload", "eg"),
                                justified = TRUE, status = "primary",
                                checkIcon = list(
                                    yes = icon("ok", lib = "glyphicon"),
                                    no = icon(""))
                            ),
                            dynamicFile(id = ns("df_upload")),
                            selectizeInput(
                                inputId = ns("df_delim"),
                                label = "File delimiter",
                                choices = c(`,`=",", Tab="\t", space=" ",
                                            `|`="|", `:`=":", `;`=";"),
                                options = list(style = "btn-primary")
                            ),
                            clearableTextInput(
                                ns("df_comment"), "File comments", value = "#")
                        ),
                        boxPlus(
                            closable = FALSE, width = 12,
                            title = "Confirm to use this table",
                            actionButton(ns("add_df"), "Confirm")
                        )
                    ),
                    boxPlus(
                        closable = FALSE, width = 9,
                        DT::DTOutput(ns("df_out"))
                    )
                )
            )%>%
            bsplus::bs_append(
                title = "2. Make a ggplot",
                div(
                    class = "esq",
                    div(
                        id = ns("esq_container"),
                        style = "display:none",
                        fluidRow(
                            canvasBtn(ns("esq-plooooooot"))
                        ),
                        esquisse::esquisserUI(
                            id = ns("esq"),
                            header = FALSE,
                            choose_data = FALSE,
                            container = esquisse::esquisseContainer(height = "700px")
                        )
                    ),
                    div(
                        id = ns("esq_disable"),
                        h3("Confirm to use a data table first",
                           class = "text-center text-warning")
                    )

                )
            ),
        spsHr(),
        hexPanel(ns("poweredby"), "THIS MODULE IS POWERED BY:",
                 hex_imgs = c(
                     "img/sps_small.png",
                     "https://github.com/dreamRs/esquisse/raw/master/man/figures/logo_esquisse.png"
                 ),
                 hex_titles = c("SystemPipeShiny", "esquisse"),
                 hex_links = c(
                     "https://github.com/systemPipeR/systemPipeShiny/",
                     "https://github.com/dreamRs/esquisse"
                 ),
                 ys = c("-20", "-18"),
                 xs = c("-10", "-11")
        )
    )
}

## server for vs_esq
#' @importFrom esquisse esquisserServer
vs_esqServer <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- "vs_esq"
        # load table ----
        df_path <- dynamicFileServer(input, session, id = "df_upload")
        observeEvent(input$source_df, {
            shinyjs::toggleElement(id = "df_upload", anim = TRUE)
        })

        data_df <- reactive({
            loadDF(
                choice = input$source_df,
                upload_path =  df_path()$datapath,
                delim = input$df_delim,
                data_init = data.frame(),
                comment = input$df_comment,
                eg_path = file.path("data", "iris.csv")
            )
        })
        # render table ----
        output$df_out <- DT::renderDT({
            DT::datatable(
                data_df(),
                style = "bootstrap",
                class = "compact",  filter = "top",
                extensions = c( 'Scroller'),
                options = list(
                    deferRender = TRUE,
                    scrollY = 200, scrollX = TRUE, scroller = TRUE,
                    columnDefs = list(list(className = 'dt-center',
                                           targets = "_all"))
                )
            )
        })
        # confirm table ----
        mydata <- reactiveValues(data = data.frame(a = 1:10, b = 10:1), name = "mydata")

        observeEvent(input$add_df, {
            # clear status on click start
            updateSpsTimeline(session, "df_status", 2, FALSE)
            shinyjs::removeCssClass("main_panel-0", "panel-success")
            shinyjs::addCssClass("main_panel-0", "panel-info")
            shinyjs::removeCssClass("main_panel-1", "panel-success")
            shinyjs::addCssClass("main_panel-1", "panel-info")
            shinyjs::hide("esq_container")
            shinyjs::show("esq_disable")
            # check
            df_filter <- data_df()[input$df_out_rows_all, ]
            spsValidate(verbose = FALSE, {
                if(!not_empty(df_filter))
                    stop("Table is empty")
                if(!nrow(df_filter) > 0)
                    stop("Table has fewer than 1 row")
                if(nrow(df_filter) < 5)
                    warning("You table has very a few rows, consider to add more")
                TRUE
            })
            # add data
            mydata$data <- df_filter
            # send success
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = ns("confirm_to_plot"),
                title = "Table added!",
                closeOnClickOutside = FALSE,
                html = TRUE,
                type = "success",
                text = div(
                    h3("Continue to make plots?"),
                    HTML("Or manually click <span class='text-info'>2. Make a ggplot</span> panel")
                )
            )
            updateSpsTimeline(session, "df_status", 2, TRUE)
            shinyjs::removeCssClass("main_panel-0", "panel-info")
            shinyjs::addCssClass("main_panel-0", "panel-success")
            shinyjs::removeCssClass("main_panel-1", "panel-info")
            shinyjs::addCssClass("main_panel-1", "panel-success")
            shinyjs::show("esq_container")
            shinyjs::hide("esq_disable")
        }, ignoreInit = TRUE)

        # jump to plotting
        observeEvent(input$confirm_to_plot, {
            req(input$confirm_to_plot)
            shinyjs::runjs(paste0("$('#", ns(""), "main_panel-1-heading > h4').trigger('click');"))
        })

        # start esquisse
        callModule(module = esquisse::esquisserServer, id = "esq", data = mydata)
    }
    moduleServer(id, module)
}
