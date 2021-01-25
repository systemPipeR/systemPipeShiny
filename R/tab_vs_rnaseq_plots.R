# plot tab template
# tagList(
#     renderDesc(ns("desc"), desc),
#     fluidRow(
#         column(
#             3,
#             div(
#                 class = "panel panel-info",
#                 id = ns("panel_left"),
#                 style = "min-height: 500px;",
#                 div(
#                     id = "",
#                     class = "panel-heading",
#                     h4(class = "panel-title", "Plot control")
#                 ),
#                 div(
#                     class = "panel-body",
#                     style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
#                     fluidRow(
#                         style = 'margin-top: 25px;',
#                         class = "text-center",
#                         canvasBtn(ns('plot_main'))
#                     ),
#                     spsHr(),
#                     fluidRow(
#                         class = "center-child",
#                         p("")
#                     ) %>%
#                         bsHoverPopover(
#                             "tip title",
#                             "tip text",
#                             placement = "bottom"
#                         )
#                 )
#             )
#         ),
#         column(
#             9,
#             div(
#                 class = "panel panel-info",
#                 id = ns("panel_right"),
#                 style = "min-height: 500px;",
#                 div(
#                     id = "",
#                     class = "panel-heading",
#                     h4(class = "panel-title", "XX plot")
#                 ),
#                 div(
#                     class = "panel-body",
#                     style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
#                     shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_main')))
#                 )
#             )
#         ),
#         heightMatcher(ns("panel_left"), ns("panel_right"))
#     )
# )

############ vs_rnaseq_glm sub tab ####################
vs_rnaseq_glmUI <- function(id){
    ns <- NS(id)
    desc <-
        '
    ## GLM-PCA
    generalized principal component analysis (GLM-PCA) for dimension
    reduction of non-normally distributed data can be plotted with the
    `GLMplot` function. This option does not offer
    transformation or normalization of raw data.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        fluidRow(
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
                            canvasBtn(ns('plot_main'))
                        ),
                        spsHr(),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("point_size"),
                                label = "Point Size",
                                min = 1,
                                max = 10,
                                step = 1,
                                value = 2,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover(
                                "Point Size",
                                "How large should the points be? 1-10",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("plot_title"),
                                label = "Plot title",
                                value = "Generalized PCA (GLM-PCA)"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Plot title",
                                "Type your plot title",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("title_size"),
                                label = "Plot title Size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 20,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Plot title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("xlab"),
                                label = "X axis label",
                                value = "Dim 1"
                            )
                        ) %>%
                            bsHoverPopover(
                                "X axis label",
                                "Type your X axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("xlab_size"),
                                label = "X axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("X axis  title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("ylab"),
                                label = "Y axis label",
                                value = "Dim 2"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Y axis label",
                                "Type your Y axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("ylab_size"),
                                label = "Y axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Y axis  title size", "", placement = "top")
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
                        h4(class = "panel-title", "GLM-PCA Plot")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_main')))
                    )
                )
            ),
            heightMatcher(ns("panel_left"), ns("panel_right"))
        )
    )
}

#' @importFrom ggplot2 ggplot aes aes_string geom_point coord_fixed ggtitle ggsave
#' @importFrom glmpca glmpca
#' @importFrom plotly ggplotly
vs_rnaseq_glmServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "glm"

        output$plot_main <- renderPlotly({
            shiny::validate(
                need(shared$rnaseq$trans_method == "raw", message = "Need to use raw transformation"),
                need(not_empty(shared$rnaseq$trans_table), message = "Count table not transformed")
            )
            shinyCatch(blocking_level = "error", {
                count_mat <- shared$rnaseq$trans_table
                factors <-  shared$rnaseq$condition
                ## glmpca is performed on raw counts
                nozero <- count_mat[which(rowSums(count_mat) > 0), ]
                gpca <- glmpca::glmpca(nozero, L=2)
                gpca.dat <- gpca$factors
                gpca.dat$condition <- factors
                Sample <- factors
                p1 <- ggplot2::ggplot(gpca.dat, ggplot2::aes(dim1, dim2)) +
                    ggplot2::geom_point(size = input$point_size, ggplot2::aes(color=Sample)) + ggplot2::coord_fixed() +
                    ggplot2::ggtitle(input$plot_title) +
                    ggplot2::xlab(input$xlab) +
                    ggplot2::ylab(input$ylab) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        plot.title = ggplot2::element_text(size = input$title_size, hjust = 0.5),
                        axis.title.x = ggplot2::element_text(size = input$xlab_size),
                        axis.title.y = ggplot2::element_text(size = input$ylab_size)
                    )
                plotly::ggplotly(p1)
            })
        })
    }
    moduleServer(id, module)
}

############ vs_rnaseq_pca sub tab ####################
vs_rnaseq_pcaUI <- function(id){
    ns <- NS(id)
    desc <-
    '
    ## PCA
    A Principal Component Analysis (PCA) plot can be created using the `PCAplot`
    function which uses the `DESeq2` package. The input data frame can be
    transformed with the `rlog` or Variance-stabilizing Transformation (`vst`)
    methods from the `DESeq2` package, or can be done without transformation.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        fluidRow(
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
                            canvasBtn(ns('plot_main'))
                        ),
                        spsHr(),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("point_size"),
                                label = "Point Size",
                                min = 1,
                                max = 10,
                                step = 1,
                                value = 2,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover(
                                "Point Size",
                                "How large should the points be? 1-10",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("plot_title"),
                                label = "Plot title",
                                value = "Principal Component Analysis (PCA)"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Plot title",
                                "Type your plot title",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("title_size"),
                                label = "Plot title Size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 20,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Plot title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("xlab"),
                                label = "X axis label",
                                value = "PC1"
                            )
                        ) %>%
                            bsHoverPopover(
                                "X axis label",
                                "Type your X axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("xlab_size"),
                                label = "X axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("X axis  title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("ylab"),
                                label = "Y axis label",
                                value = "PC2"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Y axis label",
                                "Type your Y axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("ylab_size"),
                                label = "Y axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Y axis  title size", "", placement = "top")
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
                        h4(class = "panel-title", "GLM-PCA Plot")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_main')))
                    )
                )
            ),
            heightMatcher(ns("panel_left"), ns("panel_right"))
        )
    )
}

#' @importFrom DESeq2 plotPCA
vs_rnaseq_pcaServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "pca"
        output$plot_main <- renderPlotly({
            shiny::validate(
                need(shared$rnaseq$trans_method %in% c("rlog", "vst"), message = "Need to use rlog or vst transformation"),
                need(not_empty(spsRNA_trans), message = "Count table not transformed")
            )
            shinyCatch(blocking_level = "error", {
                pcaData <- DESeq2::plotPCA(spsRNA_trans, intgroup = "condition", returnData = TRUE)
                percentVar <- round(100 * attr(pcaData, "percentVar"))
                Sample <- shared$rnaseq$condition

                p1 <- ggplot2::ggplot(pcaData, ggplot2::aes(PC1, PC2)) +
                    ggplot2::geom_point(size = input$point_size, ggplot2::aes(color=Sample)) +
                    ggplot2::coord_fixed() +
                    ggplot2::ggtitle(input$plot_title) +
                    ggplot2::xlab(paste0(input$xlab, " ", percentVar[1],"% variance")) +
                    ggplot2::ylab(paste0(input$ylab, " ", percentVar[2],"% variance")) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        plot.title = ggplot2::element_text(size = input$title_size, hjust = 0.5),
                        axis.title.x = ggplot2::element_text(size = input$xlab_size),
                        axis.title.y = ggplot2::element_text(size = input$ylab_size)
                    )
                plotly::ggplotly(p1)
            })
        })
    }
    moduleServer(id, module)
}
############ vs_rnaseq_mds sub tab ####################
vs_rnaseq_mdsUI <- function(id){
    ns <- NS(id)
    desc <-
        '
    ## MDS
    A Multidimensional Scaling (MDS) plot can be created using the `MDSplot`
    function. The input data frame can be transformed with either the `rlog` or
    Variance-stabilizing Transformation (`vst`) methods from the `DESeq2`
    package. From the input data, it computes a spearman correlation-based
    distance matrix and performs MDS analysis on it.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        fluidRow(
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
                            canvasBtn(ns('plot_main'))
                        ),
                        spsHr(),
                        fluidRow(
                            class = "center-child",
                            selectizeInput(
                                inputId = ns("cor_method"),
                                label = "Correlation Method",
                                choices = c("pearson", "kendall", "spearman"),
                                width = "100%"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Correlation Method",
                                'one of \"pearson\" (default), \"kendall\", or \"spearman\"',
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("point_size"),
                                label = "Point Size",
                                min = 1,
                                max = 10,
                                step = 1,
                                value = 2,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover(
                                "Point Size",
                                "How large should the points be? 1-10",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("plot_title"),
                                label = "Plot title",
                                value = "Multidimensional Scaling (MDS)"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Plot title",
                                "Type your plot title",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("title_size"),
                                label = "Plot title Size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 20,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Plot title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("xlab"),
                                label = "X axis label",
                                value = "X1"
                            )
                        ) %>%
                            bsHoverPopover(
                                "X axis label",
                                "Type your X axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("xlab_size"),
                                label = "X axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("X axis  title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("ylab"),
                                label = "Y axis label",
                                value = "X2"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Y axis label",
                                "Type your Y axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("ylab_size"),
                                label = "Y axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Y axis  title size", "", placement = "top")
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
                        h4(class = "panel-title", "GLM-PCA Plot")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_main')))
                    )
                )
            ),
            heightMatcher(ns("panel_left"), ns("panel_right"))
        )
    )
}
vs_rnaseq_mdsServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "mds"
        output$plot_main <- renderPlotly({
            shiny::validate(
                need(shared$rnaseq$trans_method %in% c("rlog", "vst"), message = "Need to use rlog or vst transformation"),
                need(not_empty(spsRNA_trans), message = "Count table not transformed")
            )
            shinyCatch(blocking_level = "error", {

                d <- stats::cor(SummarizedExperiment::assay(spsRNA_trans), method = input$cor_method)
                distmat <- stats::dist(1 - d)
                ## perform MDS
                mdsData <- data.frame(stats::cmdscale(distmat))
                mds <- cbind(mdsData, as.data.frame(SummarizedExperiment::colData(spsRNA_trans)))
                Sample <- shared$rnaseq$condition

                p1 <- ggplot2::ggplot(mdsData, ggplot2::aes(X1, X2)) +
                    ggplot2::geom_point(size = input$point_size, ggplot2::aes(color=Sample)) + ggplot2::coord_fixed() +
                    ggplot2::ggtitle(input$plot_title) +
                    ggplot2::xlab(input$xlab) +
                    ggplot2::ylab(input$ylab) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        plot.title = ggplot2::element_text(size = input$title_size, hjust = 0.5),
                        axis.title.x = ggplot2::element_text(size = input$xlab_size),
                        axis.title.y = ggplot2::element_text(size = input$ylab_size)
                    )
                plotly::ggplotly(p1)
            })
        })
    }
    moduleServer(id, module)
}

############ vs_rnaseq_heatmapsub tab ####################
vs_rnaseq_heatmapUI <- function(id){
    ns <- NS(id)
    desc <-
        '
    ## Heatmap
    A heatmap of the results of hierarchical clustering performed with the
    `hclust` function can be created with the `heatMaplot` function. The
    sample-wise Spearman correlation coefficients are computed before
    hierarchical clustering. The count data frame can be transformed with the
    `rlog` or Variance-stabilizing Transformation (`vst`) methods from the
    `DESeq2` package.

    Heatmap by using a list of genes is provided in the `DEG report` subtab. Please
    use `Normalize Data` subtab to create calculate some DEGs and then go to
    `DEG report` to make a heatmap over there.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        fluidRow(
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
                            canvasBtn(ns('plot_main'))
                        ),
                        spsHr(),
                        fluidRow(
                            class = "center-child",
                            numericInput(
                                inputId = ns("tree_rows"),
                                label = "Cut tree by rows",
                                min = 1,
                                max = 1000,
                                step = 1,
                                value = 1,
                                width = "100%"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Cut tree by rows",
                                "How many branches should it cut the tree by rows",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            numericInput(
                                inputId = ns("tree_cols"),
                                label = "Cut tree by columns",
                                min = 1,
                                max = 1000,
                                step = 1,
                                value = 1,
                                width = "100%"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Cut tree by columns",
                                "How many branches should it cut the tree by columns",
                                placement = "top"
                            )
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
                        h4(class = "panel-title", "GLM-PCA Plot")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        shinyjqui::jqui_resizable(plotOutput(ns('plot_main')))
                    )
                )
            ),
            heightMatcher(ns("panel_left"), ns("panel_right"))
        )
    )
}

#' @importFrom pheatmap pheatmap
#' @importFrom grid grid.draw
vs_rnaseq_heatmapServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "heatmap"
        output$plot_main <- renderImage({
            shiny::validate(
                need(shared$rnaseq$trans_method %in% c("rlog", "vst"), message = "Need to use rlog or vst transformation"),
                need(not_empty(shared$rnaseq$trans_table), message = "Count table not transformed")
            )
            outfile <- tempfile(fileext='.png')
            p1 <- shinyCatch(blocking_level = "error", {
                count_mat <- shared$rnaseq$trans_table
                anno <- as.data.frame(shared$rnaseq$condition); colnames(anno) <- "Condition"
                sampleDists <- stats::dist(t(shared$rnaseq$trans_table))
                sampleDistMatrix <- as.matrix(sampleDists)
                rownames(anno) <- colnames(sampleDistMatrix)

               pheatmap::pheatmap(
                    mat = sampleDistMatrix,
                    clustering_distance_rows = sampleDists,
                    clustering_distance_cols = sampleDists,
                    annotation_col = anno,
                    cutree_rows = input$tree_rows,
                    cutree_cols = input$tree_cols,
                    silent = TRUE
                )
            })
            png(outfile,
                width=session$clientData[[paste0('output_', ns(""), "plot_main_width")]],
                height=session$clientData[[paste0('output_', ns(""), "plot_main_height")]])
            grid::grid.draw(p1)
            dev.off()
            list(src = outfile,
                 alt = "Plot not displayed, plotting device problem")
        }, deleteFile = TRUE)
    }
    moduleServer(id, module)
}

############ vs_rnaseq_dendro sub tab ####################
vs_rnaseq_dendroUI <- function(id){
    ns <- NS(id)
    desc <-
        '
    ## Dendrogram
    A dendrogram of the results of hierarchical clustering performed with
    the `hclust` function can be created with the `hclustplot` function.
    The sample-wise Spearman correlation coefficients are computed, and then
    the results are transformed to a distance matrix before the hierarchical
    clustering is performed. The count dataframe can be transformed with the
    `rlog` or Variance-stabilizing Transformation (`vst`) methods from the
    `DESeq2` package.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        fluidRow(
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
                            canvasBtn(ns('plot_main'))
                        ),
                        spsHr(),
                        fluidRow(
                            class = "center-child",
                            selectizeInput(
                                inputId = ns("cor_method"),
                                label = "Correlation Method",
                                choices = c("pearson", "kendall", "spearman"),
                                width = "100%"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Correlation Method",
                                'one of \"pearson\" (default), \"kendall\", or \"spearman\"',
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            selectizeInput(
                                inputId = ns("layout"),
                                label = "Tree layout",
                                choices = c('rectangular', 'dendrogram', 'slanted',
                                            'ellipse', 'roundrect', 'fan',
                                            'circular', 'inward_circular',
                                            'radial', 'equal_angle',
                                            'daylight', 'ape'),
                                width = "100%"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Correlation Method",
                                "one of 'rectangular', 'dendrogram', 'slanted',
                                'ellipse', 'roundrect', 'fan', 'circular',
                                'inward_circular', 'radial', 'equal_angle',
                                'daylight' or 'ape'",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("plot_title"),
                                label = "Plot title",
                                value = "Dendrogram of count table"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Plot title",
                                "Type your plot title",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("title_size"),
                                label = "Plot title Size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 20,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Plot title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("xlab"),
                                label = "X axis label",
                                value = ""
                            )
                        ) %>%
                            bsHoverPopover(
                                "X axis label",
                                "Type your X axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("xlab_size"),
                                label = "X axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("X axis  title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("ylab"),
                                label = "Y axis label",
                                value = ""
                            )
                        ) %>%
                            bsHoverPopover(
                                "Y axis label",
                                "Type your Y axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("ylab_size"),
                                label = "Y axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Y axis  title size", "", placement = "top")
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
                        h4(class = "panel-title", "GLM-PCA Plot")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        shinyjqui::jqui_resizable(plotOutput(ns('plot_main')))
                    )
                )
            ),
            heightMatcher(ns("panel_left"), ns("panel_right"))
        )
    )
}

#' @importFrom ape as.phylo
#' @importFrom ggtree ggtree
vs_rnaseq_dendroServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "dendro"
        output$plot_main <- renderPlot({
            shiny::validate(
                need(shared$rnaseq$trans_method %in% c("rlog", "vst"), message = "Need to use rlog or vst transformation"),
                need(not_empty(shared$rnaseq$trans_table), message = "Count table not transformed")
            )
            shinyCatch(blocking_level = "error", {
                ## cor() computes the correlation coefficient
                d <- stats::cor( shared$rnaseq$trans_table, method = input$cor_method)
                ## Hierarchical cluster analysis
                hc <- stats::hclust(stats::dist(1 - d))
                tree <- ape::as.phylo(hc)
                tree$Sample <- shared$rnaseq$condition
                ggtree::ggtree(tree, color = "steelblue", layout = input$layout) +
                    ggtree::geom_tippoint(size=0.5, alpha=0.5) +
                    ggtree::geom_tiplab() +
                    ggplot2::xlab(input$xlab) +
                    ggplot2::ylab(input$ylab) +
                    ggplot2::ggtitle(input$plot_title)+
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = input$title_size, hjust = 0.5),
                        axis.title.x = ggplot2::element_text(size = input$xlab_size),
                        axis.title.y = ggplot2::element_text(size = input$ylab_size)
                    )
            })
        })
    }
    moduleServer(id, module)
}
############ vs_rnaseq_tsne sub tab ####################
vs_rnaseq_tsneUI <- function(id){
    ns <- NS(id)
    desc <- '
    ## t-SNE plot
    A Barnes-Hut t-Distributed Stochastic Neighbor Embedding (t-SNE) plot can be created
    using the `tSNEplot` function, which uses the `Rtsne` package to
    compute t-SNE values. The function removes duplicates in the input data frame,
    performs an initial PCA step. The function also
    allows for a user-set perplexity value for the computation.

    Generally, t-SNE will be good for a large N (number of samples) and cluster
    sub types within these samples. A good application for t-SNE is single cell
    RNAseq where you usually obtain hundreds to thousands of samples.
    If the sample N is small, there are a few
    duplicates for some different treatments, and there are a lot of genes (dimensions),
    PCA can be a better option.
    '
    tagList(
        renderDesc(ns("desc"), desc),
        fluidRow(
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
                            canvasBtn(ns('plot_main'))
                        ),
                        spsHr(),
                        fluidRow(
                            class = "center-child",
                            numericInput(
                                inputId = ns("perplexity"),
                                label = "Number of perplexity",
                                min = 1,
                                max = 1000,
                                step = 1,
                                value = 3,
                                width = "100%"
                            )
                        ) %>%
                            bsHoverPopover(
                                "perplexity",
                                "perplexity should < (N samples - 1)/3",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("point_size"),
                                label = "Point Size",
                                min = 1,
                                max = 10,
                                step = 1,
                                value = 2,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover(
                                "Point Size",
                                "How large should the points be? 1-10",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("plot_title"),
                                label = "Plot title",
                                value = "t-SNE"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Plot title",
                                "Type your plot title",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("title_size"),
                                label = "Plot title Size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 20,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Plot title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("xlab"),
                                label = "X axis label",
                                value = "Dim 1"
                            )
                        ) %>%
                            bsHoverPopover(
                                "X axis label",
                                "Type your X axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("xlab_size"),
                                label = "X axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("X axis  title size", "", placement = "top"),
                        fluidRow(
                            class = "center-child",
                            clearableTextInput(
                                inputId = ns("ylab"),
                                label = "Y axis label",
                                value = "Dim 2"
                            )
                        ) %>%
                            bsHoverPopover(
                                "Y axis label",
                                "Type your Y axis label",
                                placement = "top"
                            ),
                        fluidRow(
                            class = "center-child",
                            sliderInput(
                                inputId = ns("ylab_size"),
                                label = "Y axis  title size",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 16,
                                width = "100%",
                                ticks = TRUE
                            )
                        ) %>%
                            bsHoverPopover("Y axis  title size", "", placement = "top")
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
                        h4(class = "panel-title", "GLM-PCA Plot")
                    ),
                    div(
                        class = "panel-body",
                        style = "overflow-y: auto; height: Calc(100% - 38.5px); margin: 0 10px;",
                        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns('plot_main')))
                    )
                )
            ),
            heightMatcher(ns("panel_left"), ns("panel_right"))
        )
    )
}

#' @importFrom Rtsne Rtsne
vs_rnaseq_tsneServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "tsne"
        output$plot_main <- renderPlotly({
            shiny::validate(
                need(shared$rnaseq$trans_method %in% c("rlog", "vst"), message = "Need to use rlog or vst transformation"),
                need(not_empty(shared$rnaseq$trans_table), message = "Count table not transformed")
            )
            shinyCatch(blocking_level = "error", {
                countDF_uni <- t(unique( shared$rnaseq$trans_table)) # removes duplicates and transpose matrix, samples perspective
                tsne_out <- Rtsne::Rtsne(countDF_uni, dims = 2, theta = 0.0, perplexity = input$perplexity)
                Sample <- shared$rnaseq$condition
                plotdata <- data.frame(dim1 = tsne_out$Y[,1], dim2 = tsne_out$Y[,2])

                p1 <- ggplot2::ggplot(plotdata, ggplot2::aes(dim1, dim2)) +
                    ggplot2::geom_point(size = input$point_size, ggplot2::aes(color=Sample)) + ggplot2::coord_fixed() +
                    ggplot2::ggtitle(input$plot_title) +
                    ggplot2::xlab(input$xlab) +
                    ggplot2::ylab(input$ylab) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                        plot.title = ggplot2::element_text(size = input$title_size, hjust = 0.5),
                        axis.title.x = ggplot2::element_text(size = input$xlab_size),
                        axis.title.y = ggplot2::element_text(size = input$ylab_size)
                    )
                plotly::ggplotly(p1)
            })
        })
    }
    moduleServer(id, module)
}
