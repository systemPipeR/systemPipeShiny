############ UI for rnaseq data sub tab ####################
vs_rnaseq_dataUI <- function(id) {
    ns <- NS(id)
    desc <- '
    *****
    ###  Conduct a RNA-Seq analysis workflow
    In this big module, RNAseq data can be transformed, DEGs can be calculated and
    results can be visualized by different types of plots.
    *****
    #### Required files
    You need to first upload your **Raw** count table and the **targets (metadata
    table)** with a special header.
    If you are not sure how these tables look like, it is recommended to first
    use our examples first. For the metadata file, SPS uses the [targets file{blk}](https://systempipe.org/sp/spr/gettingstarted/#structure-of-targets-file) format
    from systemPipeR. All comparisions for DEG analysis are defined in the <CMP>
    header section in the targets file.

    **Targets file**

    Preparing the targets table can be
    done on the **tagets tab** under workflow management components. This app
    will automatically parse the table and the header for you. The example is a
    simplified version of systemPipeR\'s targets file. Only the `SampleName`
    and `Factor` columns are required and will be checked. Other columns will
    not be used.

    If the table is not parsed correctly, try to change the **delimiter** or the
    **file comment character**.

    Once everything is correct, click **Confirm**. The table format will be
    examed. Then you can conitnue with the count table.

    **Count file**

    Editing the count table manually is deprecated, so all tables
    here are not allowed to be edited on this tab. However, you can
    filter data by the box below each column name.

    The first column is gene IDs, and the rest of columns are sample count values.
    Characters in columns except the first one are not allowed. Column names
    starting from the second one should be your sample names.

    You need to "**Confirm**" targets first below adding this table. All
    samples presenting in this count table but not in the `SampleName` column will
    be filtered out. So make sure the **samples in targets matche samples in count table.**

    You can filter loaded data based by the box below each column name. When
    filtering is done, click "Confirm" to add the table to the server. You will
    see the preparation status tracker becomes green as you confirm each of the
    table.

    Once everything is set, there are many difference *sub-tabs*. You
    are able to make one type of plot on each tab.

    *****
    #### Interact with plots

    1. All plots can be resized. Drag the little arrow button from the bottom-right
       corner to resize it to your desired size. Most plots can be resized to infinite
       height and width.
    2. Most plots are made with [Plotly{blk}](https://plotly.com/r/) plots. You can interact
       with the plot, choose samples, zoom in/out, etc. Please read their docs for more
       inforamtion.
    2. All plots can be **screenshotted** and a copy is sent to the **Canvas**
       by clicking the `To Canvas` button.
       Go to Canvas tab from leftside navigation bar to read more instructions there. Or,
       you can save it as an image by clicking the little down arrow button next to `To Canvas` button.
       You will see image saving options.


    *****
    #### Recommendation
    - This module contains several panel columns in some sub-tabs.
    To have the best user experience, it is recommended to have a 1080p
    (1080x1920) monitor or larger.
    - Clicking the <i class="fa fa-bars" style="padding: 5px; background-color: var(--primary); color: white;"></i>
    from SPS top banner to collapse the left-side navigator will help on smaller screens.

    '
    tagList(
        renderDesc(id = ns("desc"), desc),
        ## Progress
        h3("Confirm targets and count table", class = "text-center text-info"),
        spsTimeline(
            ns("rna_status"),
            up_labels = c("1", "2"),
            down_labels = c("Targets (metadata)", "Raw Count"),
            icons = list(
                icon("table"),
                icon("file-csv")
            ),
            completes = c(FALSE, FALSE)
        ),
        bsplus::bs_accordion(id = ns("main_panel")) %>%
            bsplus::bs_set_opts(panel_type = "info") %>%
            bsplus::bs_append(
                title = "1. Confirm to use example targets table or upload a new one",
                div(
                    fluidRow(
                        h3("Load targets table", style="padding-left: 30px;"),
                        HTML('<p style="padding-left: 30px;">Only <b>SampleName</b> and <b>Factor</b> columns are required.</p>'),
                        column(
                            3,
                            box(
                                closable = FALSE, width = 12,
                                radioGroupButtons(
                                    inputId = ns("source_targets"),
                                    label = "Choose your targets file source:",
                                    selected = "eg",
                                    choiceNames = c("Upload", "Example"),
                                    choiceValues = c("upload", "eg"),
                                    justified = TRUE, status = "primary",
                                    checkIcon = list(
                                        yes = icon("ok", lib = "glyphicon"),
                                        no = icon(""))
                                ),
                                dynamicFile(id = ns("targets_upload")),
                                selectizeInput(
                                    inputId = ns("targets_delim"),
                                    label = "File delimiter",
                                    choices = c(Tab="\t", `,`=",", space=" ",
                                                `|`="|", `:`=":", `;`=";"),
                                    options = list(style = "btn-primary")
                                ),
                                clearableTextInput(
                                    ns("targets_comment"), "File comments", value = "#")
                            ),
                            box(
                                closable = FALSE, width = 12,
                                title = "Confirm to use this table",
                                actionButton(ns("add_targets"), "Confirm")
                            )
                        ),
                        box(
                            closable = FALSE, width = 9,
                            DT::DTOutput(ns("targets"))
                        )
                    ), spsHr(),
                    fluidRow(
                        h3("Targets header", style="padding-left: 15px;"),
                        p("Header with <CMP> in targets file will be used as
                          groups for DEG analysis. The parsed comparision groups
                          are displayed on the right. Please confirm this is the
                          the desired DEG groups.", style="padding-left: 15px;"),
                        box(
                            verbatimTextOutput(ns("targets_header_raw")),
                            title = "Original header lines",
                            width = 7, closable = FALSE,
                            style = "height: 300px; overflow-y: scroll;"),
                        box(
                            verbatimTextOutput(ns("targets_header_parsed")),
                            title = "Parsed DEG comparisions",
                            width = 5, closable = FALSE,
                            style = "height: 300px; overflow-y: scroll;")
                    )
                )
            )%>%
            bsplus::bs_append(
                title = "2. Confirm to use example raw count table or upload a new one",
                div(
                    style = "background-color: #f5f5f5; margin: 5px;",
                    fluidRow(
                        h3("Load count table", style="padding-left: 30px;"),
                        column(
                            3,
                            box(closable = FALSE, width = 12,
                                    radioGroupButtons(
                                        inputId = ns("source_count"),
                                        label = "Choose your count file source:",
                                        selected = "eg",
                                        choiceNames = c("Upload", "Example"),
                                        choiceValues = c("upload", "eg"),
                                        justified = TRUE, status = "primary",
                                        checkIcon = list(
                                            yes = icon("ok", lib = "glyphicon"),
                                            no = icon(""))
                                    ),
                                    dynamicFile(id = ns("count_upload")),
                                    selectizeInput(
                                        inputId = ns("count_delim"),
                                        label = "File delimiter",
                                        choices = c(Tab="\t", `,`=",", space=" ",
                                                    `|`="|", `:`=":", `;`=";"),
                                        options = list(style = "btn-primary")
                                    ),
                                    clearableTextInput(
                                        ns("count_comment"), "File comments", value = "#")

                            ),
                            box(
                                closable = FALSE, width = 12,
                                title = "Confirm to use this count table",
                                p(
                                    "Confirm targets table first.",
                                    id = ns("warning_count"),
                                    style = "color: orange;"),
                                actionButton(
                                    ns("add_count"), "Confirm", disabled = "disabled")
                            )
                        ),
                        box(
                            closable = FALSE, width = 9,
                            DT::DTOutput(ns("count"))
                        )
                    )
                )
            )
    )
}

## server for rnaseq data sub tab
#' @importFrom tibble column_to_rownames
vs_rnaseq_dataServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "data"
        targets_path <- dynamicFileServer(input, session, id = "targets_upload")
        observeEvent(input$source_targets, {
            shinyjs::toggleElement(id = "targets_upload", anim = TRUE, condition = input$source_targets =="upload")
        })
        # targets header
        observeEvent(c(targets_path(), input$source_targets), {
            req(input$source_targets)
            if(input$source_targets == "eg"){
                targets_path <- file.path("data", "targetsPE.txt")
            } else targets_path <- targets_path()$datapath
            output$targets_header_parsed <- renderPrint({
                shiny::validate(
                    need(
                        emptyIsFalse(targets_path),
                        message = "Targets file path empty")
                )
                shiny::validate(
                    need(
                        readLines(targets_path) %>%
                            str_detect("^#.{0,}<CMP>") %>%
                            any(),
                        message = "No header starting with '# <CMP>' found"
                    )
                )
                cmp <- shinyCatch(
                    systemPipeR::readComp(file=targets_path, format="matrix", delim="-"),
                    blocking_level = "error")
                shared$rnaseq$data$cmp <- cmp
                lapply(seq_along(cmp), function(i){
                    cat("---", names(cmp)[[i]], "---\n")
                    cmp[[i]] %>%
                        apply(1, glue_collapse, sep = " vs. ") %>%
                        cat(sep = "\n")
                }) %>% invisible()
            })
            output$targets_header_raw <- renderPrint({
                shiny::validate(
                    need(not_empty(targets_path),
                         message = "Targets file path empty")
                )
                readLines(targets_path) %>%
                    {.[str_detect(., "^#.?<CMP>")]} %>%
                    cat(sep = "\n")
            })
        })
        # targets table
        data_targets <- reactive({
            data_path <- targets_path()
            pgPaneUpdate('pg', 'pg_targets', 0)
            loadDF(
                choice = input$source_targets,
                upload_path = data_path$datapath,
                delim = input$targets_delim,
                comment = input$targets_comment,
                eg_path = file.path("data", "targetsPE.txt")
            )
        })
        # render table
        output$targets <- DT::renderDT({
            data_targets <- data_targets()
            if(emptyIsFalse(data_targets[["SampleName"]])){
                data_targets[["SampleName"]] <-
                    as.factor(data_targets[["SampleName"]])
            }
            if(emptyIsFalse(data_targets[["Factor"]])){
                data_targets[["Factor"]] <-
                    as.factor(data_targets[["Factor"]])
            }
            pgPaneUpdate('pg', 'pg_targets', 50)
            DT::datatable(
                data_targets,
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
        # load count table
        observeEvent(input$source_count, {
            shinyjs::toggleElement(id = "count_upload", anim = TRUE, condition = input$source_count =="upload")
        })
        count_path <- dynamicFileServer(input, session, id = "count_upload")
        data_count <- reactive({
            data_path <- count_path()
            pgPaneUpdate('pg', 'pg_count', 0) # set data progress to 0 every reload
            loadDF(
                choice = input$source_count,
                upload_path = data_path$datapath,
                delim = input$count_delim,
                comment = input$count_comment,
                eg_path = file.path("data", "rna_raw_count.tsv")
            )
        })
        # render count
        output$count <- DT::renderDT({
            data_count <- data_count()
            pgPaneUpdate('pg', 'pg_count', 50)
            names(data_count) <-  c("Genes", names(data_count)[-1])
            DT::datatable(
                data_count,
                style = "bootstrap",
                class = "compact",  filter = "top",
                extensions = c( 'Scroller'),
                options = list(
                    deferRender = TRUE,
                    scrollY = 300, scrollX = TRUE, scroller = TRUE,
                    columnDefs = list(list(className = 'dt-center',
                                           targets = "_all"))
                )
            )
        })
        # add targets to task and checks
        observeEvent(input$add_targets, {
            # clear status on click start
            shared$rnaseq$data_ready <- FALSE
            updateSpsTimeline(session, "rna_status", 1, FALSE)
            shinyjs::removeCssClass("main_panel-0", "panel-success")
            shinyjs::addCssClass("main_panel-0", "panel-info")
            updateSpsTimeline(session, "rna_status", 2, FALSE) # also remove count status
            shinyjs::removeCssClass("main_panel-1", "panel-success")
            shinyjs::addCssClass("main_panel-1", "panel-info")
            # check
            data_targets <- data_targets()[input$targets_rows_all, ]
            spsValidate(verbose = FALSE, {
                if(!not_empty(data_targets))
                    stop("targets table is empty")
                if(!nrow(data_targets) > 0)
                    stop("targets table has fewer than 1 row")
                if(!emptyIsFalse(data_targets[['SampleName']]))
                    stop("targets needs to have 'SampleName column'")
                if(!emptyIsFalse(data_targets[['Factor']]))
                    stop("targets needs to have 'Factor column'")
                if(length(data_targets$SampleName) != length(unique(data_targets$SampleName)))
                    stop("You have duplicated sample names")
                if(is.null(shared$rnaseq$data$cmp))
                    stop("Parsed DEG comparision is empty")
                shared$rnaseq$data$cmp %>% unlist() %>% unique() %>% {
                    factors <- . %in% unique(data_targets$Factor)
                    if(!all(factors)){
                        stop(paste(.[!factors], collapse = " "),
                             " in header <CMP> comparisions but not in targets table 'Factors' column")}
                }
                TRUE
            })
            # add data to shared
            shared$rnaseq$data$targets <- data_targets
            # ui interactions
            shinyjs::hide(id = "warning_count", anim = TRUE)
            shinyjs::enable(id = "add_count")
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = ns("confirm_targets"),
                title = "Targets added!",
                closeOnClickOutside = FALSE,
                html = TRUE,
                type = "success",
                text = div(
                    h3("Continue to prepare count table?"),
                    HTML("Or manually click <span class='text-info'>2. Confirm to use example raw count table or upload a new one</span> panel")
                )
            )
            updateSpsTimeline(session, "rna_status", 1, TRUE)
            shinyjs::removeCssClass("main_panel-0", "panel-info")
            shinyjs::addCssClass("main_panel-0", "panel-success")
        }, ignoreInit = TRUE)

        # jump to count accordion
        observeEvent(input$confirm_targets, {
            req(input$confirm_targets)
            shinyjs::runjs("$('#vs_rnaseq-data-main_panel-1-heading > h4').trigger('click');")
        })

        # add count to task and checks
        observeEvent(input$add_count, {
            # clear status on click start
            shared$rnaseq$data_ready <- FALSE
            updateSpsTimeline(session, "rna_status", 2, FALSE)
            shinyjs::removeCssClass("main_panel-1", "panel-success")
            shinyjs::addCssClass("main_panel-1", "panel-info")
            # check
            data_count <- data_count()[input$count_rows_all, ]
            spsValidate(verbose = FALSE, {
                if(not_empty(data_count)) TRUE
                else stop("Count table is empty")
                if(nrow(data_count) > 0) TRUE
                else stop("Count table has fewer than 1 row")
                if(ncol(data_count) > 1) TRUE
                else stop("Count table has fewer than 2 columns")
            })
            spsValidate(verbose = FALSE, {
                if((lapply(data_count, class) %>% unlist %>% .[-1] %>%
                    unique() %>% {. %in% "numeric"} %>% all())) TRUE
                else{
                    stop(
                        "All columns in count table ",
                        "except the first one must be numeric. ",
                        "Try to change delimiter or comments")
                }
            })
            samplenames <- names(data_count)[-1]
            valid_names <- samplenames %in% shared$rnaseq$data$targets[['SampleName']]
            if(!all(valid_names)){
                shinyCatch(warning(
                    "Samples '",
                    paste(samplenames[!valid_names], collapse = " "),
                    "' are not in targets table but in count table, ",
                    "will be filtered out from count table"
                ))
            }
            # filter by targets sample names and user filters
            count_filtered <-
                dplyr::select(data_count, 1, which(valid_names) + 1) %>%
                .[input$count_rows_all, ]
            # add data to shared
            shared$rnaseq$data$count <-
                tibble::column_to_rownames(count_filtered, var = "Genes") %>%
                as.matrix()
            # popup
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = ns("confirm_count"),
                title = "Count table added!",
                closeOnClickOutside = FALSE,
                html = TRUE,
                type = "success",
                text = div(
                    h3("Continue to normalize data?"),
                    HTML("Or manually click <span class='text-info'>'Normalize data'</span> on the top panel")
                )
            )
            updateSpsTimeline(session, "rna_status", 2, TRUE)
            shinyjs::removeCssClass("main_panel-1", "panel-info")
            shinyjs::addCssClass("main_panel-1", "panel-success")
            shared$rnaseq$panel_enable <- 1
            shared$rnaseq$data_ready <- TRUE
        })
        # jump to count accordion
        observeEvent(input$confirm_count, {
            req(input$confirm_count)
            shared$rnaseq$panel_selected <- "1"
        })
    }
    moduleServer(id, module)
}
