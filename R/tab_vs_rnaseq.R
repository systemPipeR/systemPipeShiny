###################### SPS RNAseq tab######################
## creation date: 2020-10-01 14:35:44
## Author:

################# UI and server for rnaseq at top level #######
vs_rnaseqUI <- function(id){
    ns <- NS(id)
    tagList(
        tags$head(
            tags$script(src="sps/js/sps_rnaseq.js"),
            tags$link(rel="stylesheet", href = "sps/css/sps_rnaseq.css")
        ),
        tabTitle("RNAseq analysis"),
        tabsetPanel(
            id = ns("main_panel"),
            tabPanel("Load Data", value = "0", vs_rnaseq_dataUI(ns("data"))),
            tabPanel("Normalize Data", value = "1", vs_rnaseq_normalUI(ns("normal"))),
            tabPanel("Plot GLM-PCA", value = "2", vs_rnaseq_glmUI(ns("glm"))),
            tabPanel("Plot PCA", value = "3", vs_rnaseq_pcaUI(ns("pca"))),
            tabPanel("Plot MDS", value = "4", vs_rnaseq_mdsUI(ns("mds"))),
            tabPanel("Plot Heatmap", value = "5", vs_rnaseq_heatmapUI(ns("heatmap"))),
            tabPanel("Plot Dendro", value = "6", vs_rnaseq_dendroUI(ns("dendro"))),
            tabPanel("Plot t-SNE", value = "7", vs_rnaseq_tsneUI(ns("tsne"))),
            tabPanel("DEG report", value = "8", vs_rnaseq_degUI(ns("deg")))
        ) %>% {
            .$children[[1]] <- bsHoverPopover(
                .$children[[1]],
                "RNAseq panel",
                "All tabs except the first one is disabled on start. You need to
                confirm targets and count table to enable `Normalize Data`. Depending
                on the method of normalization, different plot options will be enabled
                later.",
                placement = "bottom"
            )
            .
        },
        br(), br(), spsHr(),
        hexPanel(ns("poweredby"), "THIS TAB IS POWERED BY:",
                 hex_imgs = c(
                     "img/sps.png",
                     "https://raw.githubusercontent.com/systemPipeR/systemPipeR.github.io/main/static/images/systemPipeR.png",
                     "https://mikelove.github.io/assets/DESeq2.png"),
                 hex_titles = c("SystemPipeShiny", "SystemPipeR", "DESeq2"),
                 hex_links = c(
                     "https://github.com/systemPipeR/systemPipeShiny/",
                     "https://bioconductor.org/packages/release/bioc/html/systemPipeR.html",
                     "https://bioconductor.org/packages/release/bioc/html/DESeq2.html"
                 ),
                 ys = c("-10", "-10", "-6"),

        )
    )
}
vs_rnaseqServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "vs_rnaseq"
        # init rnaseq values
        observeEvent(1, {
            shared$rnaseq$panel_selected <- ""
            shared$rnaseq$panel_enable <- c()
            shared$rnaseq$data_ready <- FALSE
            shared$ranseq$data$count <- matrix()
            shared$ranseq$data$cmp <- list()
            shared$rnaseq$deg_ready <- FALSE
            shared$rnaseq$trans_ready <- FALSE
            shared$rnaseq$data[['targets']] <- data.frame("")
            shared$rnaseq$data[['deg_tables']] <- NULL
            shared$rnaseq$data[['deg_summary']] <- NULL
            shared$rnaseq$data[['trans_table']] <- NULL
        }, once = TRUE)

        vs_rnaseq_dataServer("data", shared)
        vs_rnaseq_normalServer("normal", shared)
        vs_rnaseq_glmServer("glm", shared)
        vs_rnaseq_pcaServer("pca", shared)
        vs_rnaseq_mdsServer("mds", shared)
        vs_rnaseq_heatmapServer("heatmap", shared)
        vs_rnaseq_dendroServer("dendro", shared)
        vs_rnaseq_tsneServer("tsne", shared)
        vs_rnaseq_degServer("deg", shared)
        # change tab panel
        observeEvent(shared$rnaseq$panel_selected, {
            req(emptyIsFalse(shared$rnaseq$panel_selected))
            updateTabsetPanel(
                session, "main_panel",
                selected = shared$rnaseq$panel_selected
            )
        })
        # enable panel tabs
        observeEvent(shared$rnaseq$panel_enable, {
            req(emptyIsFalse(shared$rnaseq$panel_enable))
            req(is.numeric(shared$rnaseq$panel_enable))
            enableRnaPanel(shared$rnaseq$panel_enable)
        })
    }
    moduleServer(id, module)
}

enableRnaPanel <- function(index, session = shiny::getDefaultReactiveDomain()){
    session$sendCustomMessage(
        type = "sps-rna-panel",
        message = list(
            index = as.list(index)
        )
    )
}
