###################### SPS RNAseq DEG report tab######################
## creation date: 2020-10-01 14:35:44
## Author:

################# UI and server for rnaseq deg  #################
vs_rnaseq_degUI <- function(id){
    ns <- NS(id)
    desc <-
    '
    ### DEG report
    If you have reached to this tab, some DEG results has been generated. Let us
    discover more from this tab.

    1. First check the *DEG summary* section to see if the calculation gives you
    the desired gene list. Sometimes if you set the `log fold change filter` too
    high or the `FDR` filter too low, there will be no genes left. If this is the
    case, you have the chance to refilter the DEGs. Just change the settings and
    click **Refilter**. You should see results updates in *DEG summary* section.
    2. Once you are satisfied with filters, you can make a `volcano plot`  a
    `upset plot` and a `Bland-Altman plot (MA) plot` in the lower part of this
    tab.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        # actionButton(ns("set"), "set"),
        fluidRow(
            # summary panel left ------
            column(
                3,
                div(
                    class = "panel panel-info",
                    id = ns("summary-left"),
                    style = "min-height: 400px; overflow-x: auto;",
                    div(
                        id = "",
                        class = "panel-heading",
                        h4(class = "panel-title", "Refilter DEGs")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
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
                                placement = "bottom"
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
                                placement = "bottom"
                            ),
                        spsHr(),
                        fluidRow(
                            style = 'margin-top: 25px;',
                            class = "text-center",
                            actionButton(
                                inputId = ns("refilter"),
                                label = "Refilter"
                            )%>%
                                bsHoverPopover(
                                    "Apply filters",
                                    "New filters only apply to results on this sub-tab.
                                    The results you download in the bundle from 'Normalize
                                    data tab' still use the filters on that tab.
                                    You can download new filtered data from right-side
                                    'DEG tables'",
                                    placement = "bottom"
                                ),
                            div(id = ns("loading_refilter"), style = "display:none", spsLoader())
                        )
                    )
                )
            ),
            # summary panel right ------
            column(
                9,
                class = "panel panel-info sps-panel-nav",
                style = "min-height: 400px;",
                id = ns("summary-right"),
                tabsetPanel(
                    tabPanel(
                        title = "Summary plot",
                        class = "panel-body",
                        fluidRow(
                            class = "text-center",
                            h3("DEG summary plot", class = "text-info"),
                            canvasBtn(ns('plot_deg_sum')),
                        ),
                        spsHr(),
                        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_deg_sum'), height = "100%", width = "100%"))
                    ),
                    tabPanel(
                        title = "DEG tables",
                        class = "panel-body",
                        fluidRow(
                            class = "text-center",
                            h3("DEG summary", class = "text-info"),
                            fluidRow(
                                tags$label("Only keep genes that have passed filters in download?"),
                                shinyWidgets::switchInput(
                                    ns("down_only_filter"),
                                    value = FALSE,
                                    width = "100%",
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    onStatus = "primary",
                                    offStatus = "danger"
                                )
                            ) %>%
                                bsHoverPopover(
                                    "Only filtered",
                                    "When you download the DEG table, exclude rows
                                    that did not pass the filter?",
                                    placement = "bottom"
                                ),
                            fluidRow(
                                class = "text-center",
                                downloadButton(ns("down_table"), label =  "Download DEGs"),
                                div(id = ns("loading_down_table"), style = "display:none", spsLoader())
                            )%>%
                                bsHoverPopover(
                                    "Download DEGs",
                                    "Download DEGs of all samples all comparisions
                                    in a big table. Similar to the summary table
                                    in 'Normalize data' bundle but samples are not
                                    as column names, ggplot ready style.",
                                    placement = "bottom"
                                )
                        ),
                        spsHr(),
                        DT::dataTableOutput(ns('deg_sum_table'))
                    )
                )
            )
        ),
        fluidRow(
            class = "panel panel-info sps-panel-nav",
            tabsetPanel(
                tabPanel(
                    title = "Volcano plot",
                    class = "panel-body",
                    fluidRow(
                        column(
                            3,
                            class = "plot-control-panel",
                            h3("Volcano plot", class = "text-info"),
                            selectizeInput(
                                inputId = ns("volc_choose"),
                                label = "Choose one comparision to plot",
                                choices = c(`no group yet` = 'nothing'),
                                options = list(style = "btn-primary")
                            ),
                            spsHr(),
                            canvasBtn(ns('plot_volc'))
                        ),
                        column(1),
                        column(
                            8,
                            shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_volc'), height = "100%", width = "100%"))
                        )
                    )
                ),
                tabPanel(
                    title = "Upset plot",
                    class = "panel-body",
                    fluidRow(
                        column(
                            3,
                            class = "plot-control-panel",
                            h3("Upset plot", class = "text-info"),
                            shinyWidgets::multiInput(
                                inputId = ns('upset_choose'),
                                label = "Choose groups to include:",
                                choices = c(`No comparision yet` = 'nothing')
                            ),
                            spsHr(),
                            canvasBtn(ns('plot_upset'))
                        ),
                        column(1),
                        column(
                            8,
                            shinyjqui::jqui_resizable(plotOutput(ns('plot_upset')))
                        )
                    )
                ),
                tabPanel(
                    title = "MA plot",
                    class = "panel-body",
                    fluidRow(
                        column(
                            3,
                            class = "plot-control-panel",
                            h3("MA plot", class = "text-info"),
                            selectizeInput(
                                inputId = ns('ma_choose'),
                                label = "Choose a comparision group to plot",
                                choices = c(`No comparision yet` = 'nothing')
                            ),
                            spsHr(),
                            canvasBtn(ns('plot_ma'))
                        ),
                        column(1),
                        column(
                            8,
                            shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_ma'), height = "100%", width = "100%"))
                        )
                    )
                ),
                tabPanel(
                    title = "Heat map",
                    class = "panel-body",
                    fluidRow(
                        column(
                            3,
                            class = "plot-control-panel",
                            h3("Gene level heatmap", class = "text-info"),
                            selectizeInput(
                                inputId = ns('heat_choose'),
                                label = "Choose a DEG list to build heatmap",
                                choices = c(`No comparision yet` = 'nothing')
                            ),
                            fluidRow(
                                style = "margin: 0;",
                                tags$label("Only keep samples in this DEG comparision?"),
                                shinyWidgets::switchInput(
                                    ns("heat_only_cmp"),
                                    value = FALSE,
                                    width = "100%",
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    onStatus = "primary",
                                    offStatus = "danger"
                                )
                            ) %>%
                                bsHoverPopover(
                                    "Only Compared Samples?",
                                    "Use the DEG list to plot a heatmap and keep all
                                    samples or just samples which are used to generate
                                    this deg list?",
                                    placement = "bottom"
                                ),
                            spsHr(),
                            canvasBtn(ns('plot_heat'))
                        ),
                        column(1),
                        column(
                            8,
                            shinyjqui::jqui_resizable(plotOutput(ns('plot_heat')))
                        )
                    )
                )
                # TODO Add in later version
                # tabPanel(
                #     title = "Dispersion plot",
                #     class = "panel-body",
                #     fluidRow(
                #         class = "text-center",
                #         h3("DEG dispersion and fitting plot", class = "text-info"),
                #         canvasBtn(ns('plot_deg_disp')),
                #     ),
                #     spsHr(),
                #     shinyjqui::jqui_resizable(plotOutput(ns('plot_deg_sum')))
                # )
            )
        )
    )
}


vs_rnaseq_degServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "deg"
        # update internal summary table every time DEG is done ------
        deg_tbl <- reactiveVal()
        observeEvent(shared$rnaseq$deg_ready, {
            req(shared$rnaseq$deg_ready)
            pg <- shiny::Progress$new()
            on.exit(pg$close())
            pg$set(0, "Process DEG data from Normalize Data")
            deg_list <- shared$rnaseq$data$deg_tables
            pg$set(10, "get gene names")
            genes <- rownames(deg_list[[1]])
            pg$set(20, "Process comparisions")
            deg_list_tbl <- lapply(seq_along(deg_list), function(x){
                tibble::add_column(dplyr::as_tibble(deg_list[[x]]), cmp =names(deg_list)[x], .before = 1) %>%
                    tibble::add_column(genes = genes, .before = 1) %>%
                    dplyr::mutate(direction = dplyr::case_when(
                        log2FoldChange > 0 & pass_filter ~ 'Up',
                        log2FoldChange < 0 & pass_filter ~ 'Down',
                        TRUE ~ 'Insignificant'
                    ))
            })
            pg$set(70, "Combine all comps")
            deg_tbl(deg_list_tbl %>% dplyr::bind_rows())
            pg$set(100, "done")
        })

        ######## short cut for testing this tab ----------
        observeEvent(input$set, {
            shared$rnaseq$deg_ready <- TRUE
            shared$rnaseq$data$deg_tables <- readRDS("deg.rds")
            shared$rnaseq$data$trans_table <- trans_table
            shared$rnaseq$condition <- Sample
        })
        ########
        # refiter -------
        observeEvent(input$refilter, {
            req(deg_tbl())
            deg_tbl(
                dplyr::mutate(
                    deg_tbl(),
                    pass_filter = dplyr::if_else(
                        abs(log2FoldChange) >= input$lfc_filter & padj <= input$fdr_filter,
                        1, 0
                    ),
                    direction = dplyr::case_when(
                        log2FoldChange > 0 & pass_filter ~ 'Up',
                        log2FoldChange < 0 & pass_filter ~ 'Down',
                        TRUE ~ 'Insignificant'
                    )
                )
            )
        })

        # summary plot --------
        output$plot_deg_sum <- plotly::renderPlotly({
            req(deg_tbl())
            shinyCatch(blocking_level = "error", {
                p1 <-  deg_tbl() %>%
                    dplyr::group_by(cmp, direction) %>%
                    dplyr::filter(direction != "Insignificant") %>%
                    dplyr::summarise(count =  sum(pass_filter)) %>%
                    ggplot2::ggplot() +
                    ggplot2::geom_bar(ggplot2::aes(x = count, y = cmp, fill = direction), alpha = 0.5, stat = "identity") +
                    ggplot2::ggtitle(paste0("DEG (LFC >= ", isolate(input$lfc_filter), " & FDR <= ", isolate(input$fdr_filter), ")")) +
                    ggplot2::xlab("Gene Counts") +
                    ggplot2::ylab("Comparisions") +
                    ggplot2::scale_fill_brewer(palette="Set2") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                                   axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'))
                plotly::ggplotly(p1)
            })
        })

        # display DEG table ----------
        output$deg_sum_table <-  DT::renderDT({
            shiny::validate(need(not_empty(deg_tbl()), message = "DEG table is not ready"))
            shinyCatch(blocking_level = "error", {
                df <- deg_tbl() %>% dplyr::mutate(cmp = as.factor(cmp))
                DT::datatable(
                    df,
                    style = "bootstrap",
                    class = "compact",  filter = "top",
                    extensions = c( 'Scroller','Buttons'),
                    options = list(
                        deferRender = TRUE,
                        scrollY = 580, scrollX = TRUE, scroller = TRUE,
                        columnDefs = list(list(className = 'dt-center',
                                               targets = "_all"))
                ))
            })
        })
        # download DEG table ------
        output$down_table <- downloadHandler(
            filename = function() {
                "SPS_deg_report.csv"
            },
            content = function(filename) {
                on.exit({
                    shinyjs::hide(ns("loading_down_table"))
                    shinyjs::show(ns("down_table"))
                })
                shinyjs::hide(ns("down_table"))
                shinyjs::show(ns("loading_down_table"))
                shinyCatch({
                    down_table <-
                        if (input$down_only_filter) deg_tbl() %>% dplyr::filter(pass_filter == 1)
                        else deg_tbl()
                    write.csv(down_table, filename, quote = FALSE, row.names = FALSE)
                }, blocking_level = "error")
            }
        )

        #  plot control update --------
        cmp_old <- reactiveVal()
        observeEvent(deg_tbl(), {
            req(deg_tbl())
            req(!identical(cmp_old(), deg_tbl()$cmp))
           updateSelectizeInput(
                session, "volc_choose", choices = unique(deg_tbl()$cmp)
            )
            shinyWidgets::updateMultiInput(
                session, "upset_choose", choices = unique(deg_tbl()$cmp)
            )
            updateSelectInput(
                session, "ma_choose", choices = unique(deg_tbl()$cmp)
            )
            updateSelectInput(
                session, "heat_choose", choices = unique(deg_tbl()$cmp)
            )
            cmp_old(deg_tbl()$cmp)
        })

        # volcano plot --------
        output$plot_volc <- plotly::renderPlotly({
            req(deg_tbl())
            req(input$volc_choose != "nothing")
            shinyCatch(blocking_level = "error", {
                plot_data <- dplyr::filter(deg_tbl(), cmp == input$volc_choose)
                directions <- unique(plot_data$direction)

                colors <- c()
                if ("Down" %in% directions) colors <- c(colors, '#66c2a5') # green
                if ("Insignificant" %in% directions) colors <- c(colors, 'gray')
                if ("Up" %in% directions) colors <- c(colors, '#fccdac') # red

                if(!sum(plot_data$pass_filter)) warning("valcano plot has no gene passed the filters")

                p1 <- plot_data %>%
                    ggplot2::ggplot(ggplot2::aes(x=log2FoldChange,
                                                 y=-log10(as.numeric(padj)),
                                                 label=genes),
                                    alpha = 0.7) +
                    ggplot2::geom_point(ggplot2::aes(color = direction))+
                    ggplot2::geom_vline(xintercept = c(-isolate(input$lfc_filter), isolate(input$lfc_filter)), linetype=2) +
                    ggplot2::geom_hline(yintercept = -log10(isolate(input$fdr_filter)), linetype = 2) +
                    ggplot2::ggtitle(paste('Volcano plot', input$volc_choose)) +
                    ggplot2::xlab("log2 fold change") +
                    ggplot2::ylab("-log10(p-adjust value)") +
                    ggplot2::scale_colour_manual(values = colors)+
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                                   axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'))

                plotly::ggplotly(p1) #%>% plotly::layout(autosize = F, margin = m)
            })
        })

        # upseet plot --------
        output$plot_upset <- renderPlot({
            req(deg_tbl())
            shiny::validate(need(length(input$upset_choose) > 1, message = "Choose more than one group"))
            plot_data <- deg_tbl() %>% dplyr::filter(pass_filter == 1)
            up_list <- lapply(input$upset_choose, function(x){
                dplyr::filter(plot_data, cmp == x) %>% dplyr::pull(genes)
            })
            names(up_list) <- input$upset_choose
            shinyCatch(blocking_level = "error", {
                UpSetR::upset(UpSetR::fromList(up_list), order.by="freq")
            })
        })

        # MA plot --------
        output$plot_ma <- plotly::renderPlotly({
            req(deg_tbl())
            req(input$ma_choose != "nothing")
            shinyCatch(blocking_level = "error", {
                plot_data <- dplyr::filter(deg_tbl(), cmp == input$ma_choose) %>%
                    tidyr::drop_na()

                directions <- unique(plot_data$direction)
                colors <- c()
                if ("Down" %in% directions) colors <- c(colors, '#66c2a5')
                if ("Insignificant" %in% directions) colors <- c(colors, 'gray')
                if ("Up" %in% directions) colors <- c(colors, '#fccdac')
                if(!sum(plot_data$pass_filter)) warning("MA plot has no gene passed the filters")

                p1 <- plot_data %>%
                    ggplot2::ggplot(ggplot2::aes(x = baseMean, y = log2FoldChange)) +
                    ggplot2::geom_hline(yintercept = 0, linetype=1) +
                    ggplot2::geom_hline(yintercept = c(-isolate(input$lfc_filter), isolate(input$lfc_filter)), linetype=2) +
                    ggplot2::geom_point(ggplot2::aes(colour = direction), size = 0.5) +
                    ggplot2::scale_colour_manual(values = colors) +
                    ggplot2::scale_x_continuous(trans = "log10", limits = c(0.1,300000))+
                    ggplot2::ggtitle(paste0("Bland-Altman plot of ", input$ma_choose))+
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                                   axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'))
                # m = list(
                #     l = 30,
                #     r = 100,
                #     b = 20,
                #     t = 50,
                #     pad = 0
                # )
                suppressWarnings(plotly::ggplotly(p1)) #%>% plotly::layout(autosize = F, margin = m))
            })
        })

        # heatmap plot --------
        output$plot_heat <- renderImage({
            shiny::validate(
                # need(not_empty(shared$rnaseq$trans_table), message = "Use `Normalize Data` tab to transform count table first"),
                need(input$heat_choose != "nothing", message = "choose a DEG list"),
                need(deg_tbl(), message = "Reqiure a DEG table"),
                need(shared$rnaseq$trans_ready, message = "Reqiure count transformation. Do it in `Normalize Data`")
            )
            outfile <- tempfile(fileext='.png')
            p1 <- shinyCatch(blocking_level = "error", {
                # find deg list
                degs <- deg_tbl() %>%
                    dplyr::filter(
                        cmp == input$heat_choose,
                        pass_filter == 1
                    ) %>%
                    pull(genes)
                if(length(degs) < 2) stop("Cannot plot heatmap, need more than 1 gene to pass filter")
                anno <- as.data.frame(shared$rnaseq$condition); colnames(anno) <- "Conditions"
                countmat <- shared$rnaseq$trans_table[rownames(shared$rnaseq$trans_table) %in% degs, ]
                rownames(anno) <- colnames(countmat)
                # find samples and filter matrix
                if(input$heat_only_cmp){
                    keep_conditions <- stringr::str_split(input$heat_choose, "_") %>% unlist()
                    anno <- anno[anno$Conditions %in% keep_conditions, , drop = FALSE]
                    keep_samples <- rownames(anno)
                    countmat <- countmat[, colnames(countmat) %in% keep_samples]
                }
                # find rows with the same value across all samples
                identical_row <- which(rowSums(countmat)/ncol(countmat) == countmat[, 1])
                if(length(identical_row)){
                    spswarn(c("Heatmap: rows with the same value across all samples: ", glue_collapse(identical_row, ","), " remove"))
                    countmat <- countmat[-identical_row, ]
                }
                if(nrow(countmat) < 2) stop("Heatmap: less than 2 rows (genes), stop")

                pheatmap::pheatmap(
                    countmat,
                    scale = "row",
                    clustering_distance_rows = "correlation",
                    clustering_distance_cols = "correlation",
                    annotation_col = anno,
                    silent = TRUE
                )
            })
            png(outfile,
                width=session$clientData[[paste0('output_', ns(""), "plot_heat_width")]],
                height=session$clientData[[paste0('output_', ns(""), "plot_heat_height")]])
            grid::grid.draw(p1)
            dev.off()
            list(src = outfile,
                 alt = "Plot not displayed, plotting device problem")
        }, deleteFile = TRUE)
        # TODO disp plot --------
    }
    moduleServer(id, module)
}

