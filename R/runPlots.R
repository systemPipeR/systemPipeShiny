#' @title exploreDDS
#' @description Convenience wrapper function to transform raw read counts using the `DESeq2` 
#' package transformations methods. The input file has to contain all the 
#' genes, not just differentially expressed ones. 
#'
#' @param countMatrix `date.frame` or `matrix` containing raw read counts.
#' @param targets targets `data.frame`.
#' @param cmp `character matrix` where comparisons are defined in two columns. 
#' This matrix should be generated with the [systemPipeR::readComp()] function from the targets file. 
#' Values used for comparisons need to match those in the `Factor` column of the targets file. 
#' @param preFilter allows removing rows in which there are very few reads. 
#' Accepts a numeric value with the minimum of total reads to keep. Default is `NULL`.
#' @param transformationMethod a character string indicating which transformation
#'  method it will be used on the raw read counts. Supported methods include 
#'  `rlog` and `vst` using the `DESeq2` package or default `raw` 
#'  for no data transformation.
#'  
#' @details Note that you can use the resulting transformed values in the 
#' `transformationMethod` argument only for visualization and clustering, 
#' not for differential expression analysis which needs raw counts. Users are 
#' strongly encouraged to consult the `DESeq2` vignette for more detailed 
#' information on this topic and how to properly run `DESeq2` on data sets 
#' with more complex experimental designs.
#' 
#' @references For more details on `DESeq2`, please consult the following 
#' page: http://bioconductor.org/packages/release/bioc/html/DESeq2.html
#' For more details on `targets` file definition, please consult the following 
#' page: http://www.bioconductor.org/packages/release/bioc/vignettes/systemPipeR/inst/doc/systemPipeR.html#25_structure_of_targets_file
#' 
#'@author Daniela Cassol
#' 
#' @return returns an object of class `DESeqTransform`.
#' 
#' @examples
#' suppressPackageStartupMessages({library(systemPipeR)})
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Run
#' exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="raw")
#' exploredds 
#' 
#' @export 
#' @importFrom DESeq2 DESeqDataSetFromMatrix counts DESeq rlog varianceStabilizingTransformation
exploreDDS <- function(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="raw") {
    ## A few validations ##
    if (!transformationMethod %in% c("raw", "rlog", "vst")) 
        stop("Supported methods include 'raw', 'rlog' and 'vst'")
    if(is.data.frame(countMatrix)){
        countMatrix <- as.matrix(countMatrix)
    } else if(is.matrix(countMatrix)){
        countMatrix <- countMatrix
    } else {
        stop("countMatrix needs to be assigned an object of class 'data.frame' OR 'matrix'")
    }
    if (!is.data.frame(targets)) stop("targets needs to be assignes an object of class 'data.frame'")
    if (all(class(cmp) != "matrix" & length(cmp)==2)) cmp <- t(as.matrix(cmp))
    ## Samples
    samples <- as.character(targets$Factor); names(samples) <- paste(as.character(targets$SampleName), "", sep="")
    ## Create full DESeqDataSet object
    dds <- DESeq2::DESeqDataSetFromMatrix(countData=countMatrix,
                                          colData=data.frame(condition=samples), design = ~ condition)
    ## Pre-filtering
    if(!is.null(preFilter)){
        if (!is.numeric(preFilter)) stop ("'preFilter' needs to be numeric value.")
        keep <- rowSums(DESeq2::counts(dds)) >= preFilter
        dds <- dds[keep,]
    }
    ## Estimate of (i) size factors, (ii) dispersion, (iii) negative binomial GLM fitting and (iv) Wald statistics
    dds_deseq2 <- DESeq2::DESeq(dds, quiet=TRUE)
    ## Count data transformations
    if (transformationMethod == "rlog") {
        normdata <- DESeq2::rlog(dds_deseq2, blind=  TRUE)
    } else if (transformationMethod == "vst") {
        normdata <- DESeq2::varianceStabilizingTransformation(dds_deseq2, blind = TRUE)
    } else if (transformationMethod == "raw") {
        normdata <- dds
    }
    return(normdata)
}

## Usage:
# targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
# targets <- read.delim(targetspath, comment="#")
# cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
# countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
# countMatrix <- read.delim(countMatrixPath, row.names=1)
# exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="vst")
# exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, transformationMethod="rlog")
# exploredds <- exploreDDS(countMatrix, targets, cmp=cmp[[1]], preFilter=10, transformationMethod="raw")

#' @title exploreDDSplot
#' @description Scatterplot of transformed counts from two samples or grid of all samples
#'
#' @param countMatrix `date.frame` or `matrix` containing raw read counts
#' @param targets targets `data.frame`
#' @param cmp `character matrix` where comparisons are defined in two columns. 
#' This matrix should be generated with the [systemPipeR::readComp()] function from the targets file. 
#' Values used for comparisons need to match those in the `Factor` column of the targets file. 
#' @param preFilter allows removing rows in which there are very few reads. 
#' Accepts a numeric value with the minimum of total reads to keep. Default is `NULL`.
#' @param sample a character vector of two samples. Could be specified the `Factor` 
#' column name of the targets file or the position of the column in the `countMatrix`.
#' @param plotly logical: when `FALSE` (the default), the ggplot2 plot will be returned. 
#' @param savePlot logical: when `FALSE` (the default), the plot will not be saved. 
#' If `TRUE` the plot will be saved, and requires the `filePlot` argument.
#' @param filePlot file name where the plot will be saved. For more information, please consult the
#' `ggsave` function.
#'
#' @return returns an object of `ggplot2 plot`.

#' @examples
#' library(systemPipeR)
#' ## Targets file
#' targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
#' targets <- read.delim(targetspath, comment="#")
#' cmp <- systemPipeR::readComp(file=targetspath, format="matrix", delim="-")
#' ## Count table file
#' countMatrixPath <- system.file("extdata", "countDFeByg.xls", package="systemPipeR")
#' countMatrix <- read.delim(countMatrixPath, row.names=1)
#' ## Plot
#' exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, sample=c(3,4))
#' exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], sample=c("M1A", "M1B"), save = TRUE,
#'              filePlot = "transf_deseq2.pdf")
#' @export exploreDDSplot
#' @importFrom DESeq2 estimateSizeFactors counts
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows as_tibble mutate group_by do
#' @importFrom plotly plot_ly subplot
#' @importFrom SummarizedExperiment assay
exploreDDSplot <- function(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, sample, plotly = FALSE, savePlot=FALSE, filePlot=NULL) {
    ## Calculate the data transformations
    suppressWarnings({
        vst <- exploreDDS(countMatrix, targets, cmp=cmp, preFilter=preFilter, transformationMethod="vst")
        rlog <- exploreDDS(countMatrix, targets, cmp=cmp, preFilter=preFilter, transformationMethod="rlog")
        dss <- exploreDDS(countMatrix, targets, cmp=cmp, preFilter=preFilter, transformationMethod="raw")
        dss <- DESeq2::estimateSizeFactors(dss)})
    ## create dataframe with transformed values
    transform_df <- dplyr::bind_rows(
        dplyr::as_tibble(log2(DESeq2::counts(dss, normalized=TRUE)[, sample]+1)) %>%
            dplyr::mutate(transformation = "log2(x + 1)"),
        dplyr::as_tibble(SummarizedExperiment::assay(vst)[, sample]) %>% dplyr::mutate(transformation = "vst"),
        dplyr::as_tibble(SummarizedExperiment::assay(rlog)[, sample]) %>% dplyr::mutate(transformation = "rlog"))
    names <- colnames(transform_df)[1:2]
    lvl <- levels(factor(transform_df$transformation))
    ## plot
    plot <- ggplot(transform_df, aes(x = .data[[names[1]]], y = .data[[names[2]]])) + geom_hex(bins = 80) +
        coord_fixed() + facet_grid( . ~transformation) +
        xlab(names[1]) + ylab(names[2])
    if (savePlot == TRUE){
        ggsave(filePlot, scale = 0.8)
    }
    ##Return
    if (plotly == TRUE){
        plot <- transform_df %>%
            dplyr::group_by(transformation) %>%
            dplyr::do(p=plotly::plot_ly(., x = .data[[names[1]]], y = .data[[names[2]]], color = ~transformation, type = "scatter", 
                                        name= ~transformation)) %>% 
            plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
    }
    return(plot)
}

## Usage
# library(magrittr); library(ggplot2)
# exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, sample=c(3,4))
# exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], preFilter=NULL, sample=c(3,4), plotly = TRUE)
# exploreDDSplot(countMatrix, targets, cmp=cmp[[1]], sample=c("M1A", "M1B"), save = TRUE,
#                filePlot = "transf_deseq2.pdf")

