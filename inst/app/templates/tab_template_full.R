######################SPS #@tab_displayname@# tab######################
## creation date: #@crt_date@#
## Author: #@author@#

# #@tab_id@# UI
#@tab_id@#UI<- function(id){
    ns <- NS(id)
    ### change tab description ###----
    desc <- "
    This tab is created by the `spsNewTab` function.

    #### Write some Description of this tab in markdown format
    - you should ...
        1. eg 1.
        2. eg 2.
        - **Notice**: ...`this` ...


    ```
    some code demo ...
    ```
    "
    tagList(
        tabTitle("My Plot"),
        renderDesc(id = ns("desc"), desc),
        h3("Confirm dataset", class = "text-center text-info"),
        # timeline progress define ----
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
                        box(
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
                        box(
                            closable = FALSE, width = 12,
                            title = "Confirm to use this table",
                            actionButton(ns("add_df"), "Confirm")
                        )
                    ),
                    box(
                        closable = FALSE, width = 9,
                        DT::DTOutput(ns("df_out"))
                    )
                )
            )%>%
            bsplus::bs_append(
                title = "2. Make a plot",
                fluidRow(
                    div(
                        id = ns("plot_container"),
                        style = "display: none;",
                        column(
                            3,
                            div(
                                class = "panel panel-info",
                                id = ns("panel_left"),
                                style = "min-height: 500px;",
                                div(
                                    id = "",
                                    class = "panel-heading",
                                    h4(class = "panel-title", "Plot control")
                                ),
                                div(
                                    class = "panel-body",
                                    style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                                    fluidRow(
                                        style = 'margin-top: 25px;',
                                        class = "text-center",
                                        canvasBtn(ns("plot_main"))
                                    ),
                                    spsHr(),
                                    ### add your plot control UI below ### ----
                                    # this example adds a text input to allow users to modify plot title,
                                    # see server code on how we can use it
                                    shiny::fluidRow(
                                        class = "center-child",
                                        clearableTextInput(
                                            inputId = ns("plot_title"),
                                            label = "Plot title",
                                            value = "Example plot"
                                        )
                                    ) %>%
                                        bsHoverPopover("Plot title", "Type your plot title", placement = "top")
                                )
                            )
                        ),
                        column(
                            9,
                            div(
                                class = "panel panel-info",
                                id = ns("panel_right"),
                                style = "min-height: 500px;",
                                div(
                                    id = "",
                                    class = "panel-heading",
                                    h4(class = "panel-title", "My Plot")
                                ),
                                div(
                                    class = "panel-body",
                                    id = ns("plot_container"),
                                    style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                                    shinyjqui::jqui_resizable(plotOutput(ns("plot_main")))
                                ),
                            )
                        ),
                        heightMatcher(ns("panel_left"), ns("panel_right")),

                    ),
                    div(
                        id = ns("plot_disable"),
                        h3("Confirm to use a data table first",
                           class = "text-center text-warning")
                    )
                )
            ),
        spsHr(),
        hexPanel(ns("poweredby"), "THIS MODULE IS POWERED BY:",
                 hex_imgs = c(
                     "img/sps_small.png"
                 ),
                 hex_titles = c("SystemPipeShiny"),
                 hex_links = c(
                     "https://github.com/systemPipeR/systemPipeShiny/"
                 ),
                 ys = c("-20"),
                 xs = c("-10")
        )
    )
}

# #@tab_id@# server
# DO NOT change arugment `id` and `shared`
#@tab_id@#Server <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- id
        mydata <- reactiveValues(data = NULL)
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
                ### Change your example dataset ###----
                eg_path = system.file(package = "systemPipeShiny", "app", "data", "iris.csv")
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
        observeEvent(input$add_df, {
            # clear status on click start
            updateSpsTimeline(session, "df_status", 2, FALSE)
            shinyjs::removeCssClass("main_panel-0", "panel-success")
            shinyjs::addCssClass("main_panel-0", "panel-info")
            shinyjs::removeCssClass("main_panel-1", "panel-success")
            shinyjs::addCssClass("main_panel-1", "panel-info")
            shinyjs::hide("plot_container")
            shinyjs::show("plot_disable")
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
                    HTML("Or manually click <span class='text-info'>2. Make a plot</span> panel")
                )
            )
            updateSpsTimeline(session, "df_status", 2, TRUE)
            shinyjs::removeCssClass("main_panel-0", "panel-info")
            shinyjs::addCssClass("main_panel-0", "panel-success")
            shinyjs::removeCssClass("main_panel-1", "panel-info")
            shinyjs::addCssClass("main_panel-1", "panel-success")
            shinyjs::show("plot_container")
            shinyjs::hide("plot_disable")
        }, ignoreInit = TRUE)

        # jump to plotting
        observeEvent(input$confirm_to_plot, {
            req(input$confirm_to_plot)
            shinyjs::runjs(paste0("$('#", ns(""), "main_panel-1-heading > h4').trigger('click');"))
        })

        ### define your plotting code ### ----
        output$plot_main <- renderPlot({
            req(mydata$data)
            plot_data <- mydata$data
            spsValidate({
                stopifnot(inherits(plot_data, "data.frame"))                        # require a dataframe
                stopifnot(nrow(plot_data) > 1)                                      # has least one row
                if (!all(c("Sepal.Length", "Sepal.Width") %in% colnames(plot_data)))# has two required columns
                    stop("Require column 'Sepal.Length' and 'Sepal.Width'")

                TRUE # give it a TRUE if all checks passed.
            },
            verbose = FALSE # only show messages when fail
            )
            # actual plot code
            ggplot2::ggplot(plot_data) +
                ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
                # grab user defined title from plot control by `input$+control_ID`,
                # no need to add `ns()` on server end.
                ggplot2::ggtitle(input$plot_title)
        })

    }
    moduleServer(id, module)
}
