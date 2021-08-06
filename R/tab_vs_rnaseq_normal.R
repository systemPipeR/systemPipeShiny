############ vs_rnaseq_normal sub tab ####################
#' @importFrom shinyjs disabled
vs_rnaseq_normalUI <- function(id){
    ns <- NS(id)
    desc <-
    '
    ### Data Transformation
    Data transformation here is done by the **DESeq2** package. If all files
    are prepared from the "Load Data" sub tab. There are three different
    methods you can choose from:

    1. `raw`: DESeq2 `estimateSizeFactors` normalization and take log2(n + 1). This option can plot GLM-PCA
    2. `rlog`, `vst`: Read [DESeq2 docs{blk}](https://www.bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#how-do-i-use-vst-or-rlog-data-for-differential-testing)
    for the method. There are good to remove batch effects. Results can plot PCA, dendrogram, MDS, heatmap and t-SNE.
    3. If you are familar with R and want to continue other analysis after these, simple stop SPS and there is a `spsRNA_trans` object
       stored in your R environment. `raw` method gives you a normalized count table. Other two methods give you a `DESeq2` class object.
       You can use it for other analysis.
    4. If you want a excel readable file, select and download your desired files from the right-side panel.

    ### DEG analysis
    This part is also powered by **DESeq2** package. Choose your desired options
    in *2-2 Run DEG analysis*.

    A tool-tip will show up on the left side when your mouse hovers on an option.
    For full explaination of these options, read
    [DESeq2 documents{blk}](https://www.bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html).

    1. After the calculation, you can go to the *DEG report* sub-tab to filter results and make plots.
    2. Similar to count transformation, DEG analysis will also store a global object called `spsDEG`.
       It is a `summerizedExperiment` object which has all individual tables from all DEG comparisions.
       You can use it for other downstream analysis.

    '
    tagList(
        renderDesc(ns("desc"), desc),
        # actionButton(ns("set"), "set"),
        h3("Explore RNAseq data", class = "text-center text-info"),
        div(
            style = 'margin: auto; width: 500px',
            div(
                style = 'display: inline-block; vertical-align: middle;',
                spsTimeline(
                    ns("rna_status1"),
                    up_labels = c("1"),
                    down_labels = c("Options"),
                    icons = list(
                        icon("cog")
                    ),
                    completes = c(FALSE)
                )
            ),
            div(
                id = ns("rna_status2"),
                style = 'display: inline-block; border-left: 6px solid #d6dce0; border-radius: 70px;',
                fluidRow(
                    spsTimeline(
                        ns("rna_status2-1"),
                        up_labels = c("2-1"),
                        down_labels = c("Transform Data"),
                        icons = list(
                            icon("table")
                        ),
                        completes = c(FALSE)
                    )
                ),
                fluidRow(
                    spsTimeline(
                        ns("rna_status2-2"),
                        up_labels = c("2-2"),
                        down_labels = c("Run DESeq2"),
                        icons = list(
                            icon("server")
                        ),
                        completes = c(FALSE)
                    )
                )
            )
        ),
        fluidRow(
            column(
                8,
                bsplus::bs_accordion(id = ns("main_panel")) %>%
                    bsplus::bs_set_opts(panel_type = "info") %>%
                    bsplus::bs_append(
                        title = "1 Choose how you want to process data",
                        div(
                            fluidRow(
                                class = "center-child",
                                selectizeInput(
                                    inputId = ns("trans_options"),
                                    width = "100%",
                                    label = "Choose one option",
                                    choices = c(`Transform and explore the raw counts`="raw",
                                                `Take me to do DEG` = 'deg'),
                                    options = list(style = "btn-primary")
                                )
                            ),
                            fluidRow(
                                class = "center-child",
                                sliderInput(
                                    ns("prefilter"),
                                    width = "100%",
                                    label = "Prefilter",
                                    min = 0, max = 100, value = 1, ticks = TRUE, step = 1
                                )
                            )%>%
                                bsHoverPopover(
                                    "Prefilter",
                                    "Each gene should at least have following count(s).",
                                    placement = "left"
                                ),
                            spsHr(),
                            fluidRow(
                                class = "text-center",
                                actionButton(ns("confirm_process"), "Confirm")
                            )
                        )
                    ) %>%
                    bsplus::bs_append(
                        title = "2-1 Explore raw data with transformation methods",
                        div(
                            fluidRow(
                                class = "center-child",
                                selectizeInput(
                                    inputId = ns("trans_method"),
                                    width = "100%",
                                    label = "Choose transformation method",
                                    choices = c(raw="raw", VST="vst", rlog="rlog"),
                                    options = list(style = "btn-primary")
                                )
                            )%>%
                                bsHoverPopover(
                                    "Transformation method",
                                    "raw runs DESeq2::estimateSizeFactors and take log2(n + 1);
                                     VST and rlog uses variance stabilizing and rlog transformations
                                     from DESeq2. Read description section for more details.
                                    ",
                                    placement = "left"
                                ),
                            spsHr(),
                            fluidRow(
                                class = "text-center",
                                actionButton(
                                    inputId = ns("trans"),
                                    label = "Transform"),
                                div(id = ns("loading_trans"), style = "display:none", spsLoader())
                            ),
                            fluidRow(uiOutput(ns("trans_plot_opts")))
                        )
                    ) %>%
                    bsplus::bs_append(
                        title = "2-2 Run DEG analysis",
                        div(
                            fluidRow(class = "center-child text-center", h4("DEG running settings")),
                            fluidRow(
                                class = "center-child",
                                selectizeInput(
                                    inputId = ns("cmp_choice"),
                                    width = "100%",
                                    label = "Choose a comparision set to calculate DEG",
                                    choices = c(`No group yet`=""),
                                    options = list(style = "btn-primary")
                                )
                            )%>%
                                bsHoverPopover(
                                    "Comparision sets",
                                    "A large comparision set with many comparsions will be
                            very slow.",
                                    placement = "left"
                                ),
                            fluidRow(
                                class = "center-child",
                                style = "max-height: 200px; overflow-y: auto;",
                                verbatimTextOutput(ns("cmp_selected"))
                            ), br(),
                            fluidRow(
                                class = "center-child",
                                tags$label("Independent: Each comparision is treated as an independent dataset?"),
                                shinyWidgets::switchInput(
                                    ns("deg_independ"),
                                    value = FALSE,
                                    width = "100%",
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    onStatus = "primary",
                                    offStatus = "danger"
                                )
                            ) %>%
                                bsHoverPopover(
                                    "Independent",
                                    "If independent is YES then the count table will be subsetted
                                     for each comparison. This behavior can be useful when
                                     working with samples from unrelated studies. For
                                     samples from the same or comparable studies, the
                                     setting independent NO is usually preferred.",
                                    placement = "left"
                                ),
                            fluidRow(class = "center-child text-center", h4("DEG results settings")),
                            fluidRow(
                                class = "center-child",
                                tags$label("Use `lfcShrink` function to correct fold change?"),
                                shinyWidgets::switchInput(
                                    ns("lfc"),
                                    value = TRUE,
                                    width = "100%",
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    onStatus = "primary",
                                    offStatus = "danger"
                                )
                            )%>%
                                bsHoverPopover(
                                    "Shrink log2 fold changes",
                                    "Adds shrunken log2 fold changes (LFC) and SE to a
                                     results table from DESeq run",
                                    placement = "left"
                                ),
                            fluidRow(
                                class = "center-child",
                                id = ns("lfc_type_div"),
                                selectizeInput(
                                    inputId = ns("lfc_type"),
                                    width = "100%",
                                    label = "Choose lfcShrink method",
                                    choices = c("normal", "apeglm", "ashr"),
                                    options = list(style = "btn-primary")
                                )
                            ) %>%
                                bsHoverPopover(
                                    "lfcShrink Methods",
                                    "One of normal, apeglm, ashr. The apeglm publication
                                     demonstrates that 'apeglm' and 'ashr' outperform
                                     the original 'normal' shrinkage estimator.
                                     If you do not see all three options, that means you
                                     need to install addtional packages. 'apeglm' can be installed
                                     from Bioconductor. 'ashr' is a CRAN package.",
                                    placement = "left"
                                ),
                            fluidRow(
                                class = "center-child",
                                sliderInput(
                                    ns("lfc_filter"),
                                    width = "100%",
                                    label = "Log fold change filter",
                                    min = 0, max = 10, value = 0, ticks = TRUE, step = 0.1
                                )
                            ) %>%
                                bsHoverPopover(
                                    "Log fold change filter",
                                    "Filter DEGs that the absolute value of
                                    log2 fold change below this number. Default 0, no filter,
                                    but recommend at least 1 for real data.",
                                    placement = "left"
                                ),
                            fluidRow(
                                class = "center-child",
                                sliderInput(
                                    ns("fdr_filter"),
                                    width = "100%",
                                    label = "FDR filter",
                                    min = 0, max = 1, value = 1, ticks = TRUE, step = 0.01
                                )
                            ) %>%
                                bsHoverPopover(
                                    "False discovery rate filter",
                                    "Only keep DEGs that have adjusted P-value below this number.
                                     Default 1, no filter, but recommend 0.05.",
                                    placement = "left"
                                ),
                            spsHr(),
                            fluidRow(
                                style = 'margin-top: 25px;',
                                class = "text-center",
                                actionButton(
                                    inputId = ns("deg"),
                                    label = "Calculate DEG"),
                                div(id = ns("loading_deg"), style = "display:none", spsLoader())
                            ),
                            fluidRow(uiOutput(ns("deg_plot_opts")))
                        )
                    )
            ),
            column(
                4,
                div(
                    class = "panel panel-info", id = ns("res_status"),
                    div(
                        id = "",
                        class = "panel-heading",
                        h4(class = "panel-title", "Results Status")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        fluidRow(
                            tags$b("Check results you want to download:"),
                            shinyjs::disabled(checkboxInput(
                                inputId = ns("check_trans"),
                                label = HTML("<p class='text-gray'>Transformed data (<span class='text-danger'>Not ready</span>)</p>")
                            ))%>%
                                bsHoverPopover(
                                    "Transformed data table",
                                    "Include the transformed data table in the
                                    bundle. Data depends on the trans method you
                                    selected.",
                                    placement = "left"
                                ), br(),
                            shinyjs::disabled(checkboxInput(
                                inputId = ns("check_deg_csv"),
                                label = HTML("<p class='text-gray'>DEG summary data (<span class='text-danger'>Not ready</span>)</p>")
                            )) %>%
                                bsHoverPopover(
                                    "DEG summary table",
                                    "A summary table of all comparisions.
                                    comparisions are arranged in columns. If you
                                    you comparisions arranged in rows, go to
                                    'DEG Report' sub-tab and download table
                                    from there.",
                                    placement = "left"
                                ), br(),
                            shinyjs::disabled(checkboxInput(
                                inputId = ns("check_deg_rds"),
                                label = HTML("<p class='text-gray'>DEG as R data (<span class='text-danger'>Not ready</span>)</p>")
                            ))%>%
                                bsHoverPopover(
                                    "DEG as Rds",
                                    "Include the DEG comparision results as a
                                    `SummerizedExperiemnt` object in the Rds file.
                                    Useful if you want to run other Bioconductor
                                    packages.",
                                    placement = "left"
                                )
                        ),
                        spsHr(),
                        fluidRow(
                            class = "text-center",
                            downloadButton(
                                ns("down_bundle"),
                                label = "Download Bundle"
                            ),
                            div(id = ns("loading_down_bundle"), style = "display:none", spsLoader())
                        )%>%
                            bsHoverPopover(
                                "Data download",
                                "First run some analysis in '2-1' or '2-2' and
                                when the data is ready, check some data dowload
                                option(s) to enable the download button.",
                                placement = "left"
                            )
                    )
                )
            )
        ),
        absolutePanel(
            id = ns("deg_pg_panel"),
            style = "
            background-color: #ecf0f5;
            border: 2px solid #d2d6de;
            border-radius: 5%;
            display: none;
            z-index: 99;
            ",
            top = "40%",
            left = "45%",
            width = "400px",
            height = "100px",
            fixed = TRUE,
            cursor = "default",
            h4("DEG Running Porgress", style="text-align: center"), br(),
            shinyWidgets::progressBar(
                id = ns("deg_pg"), value = 0,
                title = "", total = 6
            )
        )
    )
}

vs_rnaseq_normalServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        # set up variables
        tab_id <- "normal"
        count <- reactiveVal(NULL)
        targets <- reactiveVal(NULL)
        cmp <- reactiveVal(NULL)
        # update fold change type
        observeEvent(input$lfc ,{
            shinyjs::toggleElement("lfc_type_div", anim = TRUE, condition = input$lfc)
        })
        observeEvent(1, {
            missing_pkg <- checkNameSpace(c("apeglm", "ashr"), quietly = TRUE)
            if(emptyIsFalse(missing_pkg)){
                updateSelectizeInput(
                    session, "lfc_type",
                    choices = c("normal", "apeglm", "ashr")[!c("normal", "apeglm", "ashr") %in% missing_pkg],
                    label = glue("lfcShrink methods: install R package(s): {glue_collapse(missing_pkg, ', ')} to enable additional options")
                )
            }
        }, once = TRUE)
        ####### develop shotcut
        # observeEvent(input$set, {
        #     shared$rnaseq$data_ready <- TRUE
        #     targetspath <- "data/targetsPE.txt"
        #     targets(read.delim(targetspath, comment="#"))
        #     cmp(systemPipeR::readComp(file=targetspath, format="matrix", delim="-"))
        #     count(read.delim(file.path("data", "rna_raw_count.tsv"), row.names=1) %>% as.matrix())
        # })
        #######
        # get value from data tab
        observeEvent(shared$rnaseq$data_ready, {
            req(shared$rnaseq$data_ready)
            targets(shared$rnaseq$data$targets)
            cmp(shared$rnaseq$data$cmp)
            count(shared$rnaseq$data$count)
            updateSelectInput(session, "cmp_choice", choices = names(cmp()))
        })
        # print selected cmp group
        output$cmp_selected <- renderPrint({
            validate(need(emptyIsFalse(input$cmp_choice), message = "No valid DEG group detected"))
            cat("---", input$cmp_choice, "---\n")
            cmp()[[input$cmp_choice]] %>%
                apply(1, glue_collapse, sep = " vs. ") %>%
                cat(sep = "\n")
        })
        # confirm step 1 ------------
        observeEvent(input$confirm_process , {
            updateSpsTimeline(session, "rna_status1", 1, TRUE)
            shinyjs::runjs(glue("$('#{ns('rna_status2')}').css('border-left', '6px solid #5cb85c');"))
            req(input$trans_options)
            if(input$trans_options == "raw"){
                shinyjs::runjs("$('#vs_rnaseq-normal-main_panel-1-heading > h4').trigger('click');")
            } else {
                shinyjs::runjs("$('#vs_rnaseq-normal-main_panel-2-heading > h4').trigger('click');")
            }

        })
        # step 2-1 transform -----------
        observeEvent(input$trans, {
            ## reset
            shared$rnaseq$trans_ready <- FALSE
            updateSpsTimeline(session, "rna_status2-1", 1, FALSE)
            shinyjs::disable("check_trans")
            rnaseqDataText("check_trans")
            output$trans_plot_opts <- renderUI({p("")})

            dds <- shinyCatch(blocking_level = "error", {
                    ## pair sample names and sample ID
                    shared$rnaseq$condition <- as.character(targets()$Factor)
                    names(shared$rnaseq$condition) <- paste(as.character(targets()$SampleName), "", sep="")
                    ## construct deseq2 object
                    dds <- DESeq2::DESeqDataSetFromMatrix(countData=count(), colData=data.frame(condition=shared$rnaseq$condition), design = ~ condition)
                    ## add prefilter
                    dds <- dds[rowSums(DESeq2::counts(dds)) >= input$prefilter, ]
                    ## transform method
                    switch(
                        input$trans_method,
                        "raw" = {
                            dds_normal <- DESeq2::estimateSizeFactors(dds)
                            log2(DESeq2::counts(dds_normal, normalized=TRUE) + 1)
                        },
                        "vst" = {
                            spsRNA_trans <<- DESeq2::varianceStabilizingTransformation(dds, blind = TRUE)
                            SummarizedExperiment::assay(spsRNA_trans)
                        },
                        "rlog" = {
                            spsRNA_trans <<- DESeq2::rlog(dds, blind=  TRUE)
                            SummarizedExperiment::assay(spsRNA_trans)
                        }
                    )
            })
            shared$rnaseq$trans_method <- input$trans_method
            shared$rnaseq$trans_table <- dds
            shinyWidgets::sendSweetAlert(
                session = session,
                title = "Data transformed",
                text = "Data transformed, make some plots",
                type = "success"
            )
            # enable tabs and show plot options
            req(input$trans_method)
            if(input$trans_method == "raw"){
                shared$rnaseq$panel_enable <-
                    if (shared$rnaseq$deg_ready) c(1, 2, 8)
                    else  c(1, 2)
                output$trans_plot_opts <- renderUI({
                    tagList(
                        spsHr(),
                        gallery(
                            title = paste(toupper(input$trans_method), "Plot Options"),
                            texts = c("GLM-PCA"),
                            hrefs = c("2"),
                            images = c("plot_list/plot_gpca.png"),
                            image_frame_size = 3
                        )
                    )
                })
            } else {
                shared$rnaseq$panel_enable <-
                    if (shared$rnaseq$deg_ready) c(1, 3:8)
                    else c(1, 3:7)
                output$trans_plot_opts <- renderUI({
                    tagList(
                        spsHr(),
                        gallery(
                            title = paste(toupper(input$trans_method), "transformed plot options"),
                            texts = c("PCA", "MDS", "Heatmap", "Dendrogram", "t-SNE"),
                            hrefs = as.character(3:7),
                            images = c(
                                "plot_list/plot_pca.png", "plot_list/plot_mds.png",
                                "plot_list/plot_heatmap.png", "plot_list/plot_dendro.png",
                                "plot_list/plot_tsne.png"
                            ),
                            image_frame_size = 3
                        )
                    )
                })
            }

            ## update status
            shared$rnaseq$trans_ready <- TRUE
            shinyjs::enable("check_trans")
            rnaseqDataText("check_trans", TRUE)
            updateCheckboxInput(session, "trans_ready", label = "Transformed data (ready)")
            updateSpsTimeline(session, "rna_status2-1", 1, TRUE)
        })

        # step 2-2 calc DEG ----
        observeEvent(input$deg, {
            on.exit({
                shinyjs::hideElement('deg_pg_panel', anim = TRUE)
                shinyjs::showElement("deg")
                shinyjs::hideElement("loading_deg")
            })
            ## set up progress and reset status
            shared$rnaseq$deg_ready <- FALSE
            shinyjs::disable("check_deg_csv")
            shinyjs::disable("check_deg_rds")
            updateSpsTimeline(session, "rna_status2-2", 1, FALSE)
            shinyjs::hideElement("deg")
            shinyjs::showElement("loading_deg")
            shinyjs::showElement('deg_pg_panel', anim = TRUE)
            rnaseqDataText("check_deg_csv")
            rnaseqDataText("check_deg_rds")
            output$deg_plot_opts <- renderUI({p("")}) # reset UI

            dds <- shinyCatch(.run_DESeq2(
                countDF = count(), targets = targets(),
                cmp = cmp()[[input$cmp_choice]],
                independent = input$deg_independ,
                prefilter = input$prefilter,
                lfcShrink = input$lfc,
                lfcShrink_type = input$lfc_type,
                pg_id = "deg_pg",
                lfc_filter = input$lfc_filter,
                fdr_filter = input$fdr_filter
            ), blocking_level = "error")

            ## update data
            spsDEG <<- dds[['table_list']]
            shared$rnaseq$data[['deg_tables']] <- SummarizedExperiment::assays(dds[['table_list']])
            shared$rnaseq$data[['deg_summary']] <- dds[['bigtable']]
            shared$rnaseq$deg_ready <- TRUE

            ## update status
            updateSpsTimeline(session, "rna_status2-2", 1, TRUE)
            shinyjs::enable("check_deg_csv")
            shinyjs::enable("check_deg_rds")
            rnaseqDataText("check_deg_csv", TRUE)
            rnaseqDataText("check_deg_rds", TRUE)
            updateCheckboxInput(session, "deg_ready", label = "DEG data (ready)")
            ## enable tabs and gen gallery
            if(!shared$rnaseq$trans_ready) {
                shared$rnaseq$panel_enable <- c(1, 8)
            } else if (shared$rnaseq$trans_ready& shared$rnaseq$trans_method == "raw"){
                shared$rnaseq$panel_enable <- c(1, 2, 8)
            } else if (shared$rnaseq$trans_ready) {
                shared$rnaseq$panel_enable <- (1:8)[-2]
            }

            output$deg_plot_opts <- renderUI({
                tagList(
                    spsHr(),
                    gallery(
                        title = "Make DEG reports",
                        texts = "DEG report",
                        hrefs = as.character(8),
                        images = "img/sps-rnaseq.png",
                        image_frame_size = 4
                    )
                )
            })
            ## send alert
            shinyWidgets::sendSweetAlert(
                session = session,
                title = "DEG done!",
                text = div(
                    tags$b("DEG calculated, make some plots"),
                    tags$ul(
                        tags$li(
                        "If you are running this app locally, and wish to do custom
                        analysis from the DEG results, simply stop SPS and a 'SummerizedExperiment'
                        object called 'spsDEG' is returned to your global R environment. You can
                        directly use it to run other code."
                        ),
                        tags$li(
                        "If you are not familiar with R and just want some Excel readable
                        files, download the results bundle from right side panel.
                        ")
                    )
                ),
                type = "success",
                html = TRUE
            )

        })

        # download bundle
        observeEvent(c(input$check_trans, input$check_deg_csv, input$check_deg_rds), {
            if(any(input$check_trans, input$check_deg_csv, input$check_deg_rds)) shinyjs::enable("down_bundle")
            else shinyjs::disable("down_bundle")
        })

        output$down_bundle <- downloadHandler(
            filename = function() {
                "SPS_rnaseq.zip"
            },
            content = function(filename) {
                on.exit({
                    shinyjs::hide(ns("loading_down_bundle"))
                    shinyjs::show(ns("down_bundle"))
                    pg$close()
                })
                shinyjs::hide(ns("down_bundle"))
                shinyjs::show(ns("loading_down_bundle"))
                shinyCatch({
                    if(!any(input$check_trans, input$check_deg_csv, input$check_deg_rds))
                        stop("No file to include, check some data options to download")
                }, blocking_level = "error")

                pg <- shiny::Progress$new()
                pg$set(0, message = "Check output folder")
                dir_path <- file.path(tempdir(), "SPS_rnqseq")
                files <- c("SPS_deseq2_transformed_counts.csv", "spsRNA_trans.rds", "SPS_deseq2_DEG_summary.csv", "SPS_deseq2_DEG_SummarizedExperiment.rds")
                dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
                shinyCatch({
                    if(emptyIsFalse(input$check_trans)) {
                        pg$set(20, message = "write transformed counts")
                        write.csv(shared$rnaseq$trans_table, file.path(dir_path, files[1]), quote = FALSE)
                        saveRDS(spsRNA_trans, file.path(dir_path, files[2]))
                    }
                    if(emptyIsFalse(input$check_deg_csv)) {
                        pg$set(40, message = "write DEG summary")
                        write.csv(shared$rnaseq$data[['deg_summary']], file.path(dir_path, files[3]), quote = FALSE)
                    }
                    if(emptyIsFalse(input$check_deg_rds)) {
                        pg$set(60, message = "write DEG Rds")
                        saveRDS(shared$rnaseq$data[['deg_tables']], file.path(dir_path, files[4]))
                    }
                    pg$set(70, message = "check written files")
                    if(!emptyIsFalse(list.files(dir_path))) stop("No file has been written on server")
                }, blocking_level = "error")
                pg$set(80, message = "Start to zip, please wait")
                zip::zip(
                    zipfile = filename,
                    files = file.path(dir_path, files[c(input$check_trans, input$check_trans, input$check_deg_csv, input$check_deg_rds)]),
                    mode = 'cherry-pick'
                )
            },
            contentType = "application/zip"
        )
    }
    moduleServer(id, module)
}


rnaseqDataText <- function(id, state = FALSE, session = shiny::getDefaultReactiveDomain()){
    session$sendCustomMessage("rnaseq-down-text", list(id = session$ns(id), state = state))
}
