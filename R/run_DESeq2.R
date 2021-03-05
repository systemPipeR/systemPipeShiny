.run_DESeq2 <- function (countDF, targets, cmp, independent = FALSE,
                         prefilter = 1,
                         lfcShrink = TRUE, lfcShrink_type = "normal",
                         pg_id = "", lfc_filter = 0,
                         fdr_filter = 1,
                         session = getDefaultReactiveDomain()) {
    if (all(!inherits(cmp, "matrix") & length(cmp) == 2))
        cmp <- t(as.matrix(cmp))
    samples <- as.character(targets$Factor)
    names(samples) <- paste(as.character(targets$SampleName),
                            "", sep = "")
    countDF <- countDF[, names(samples)]
    countDF[is.na(countDF)] <- 0
    deseqDF <- data.frame(row.names = rownames(countDF))
    deseqDF <- deseqDF[rowSums(deseqDF) >= as.numeric(prefilter), ]
    if (independent == TRUE) {
        loopv <- seq(along = cmp[, 1])
    }
    else {
        loopv <- 1
    }
    cmp_len <- nrow(cmp)
    deg_res <- list()
    for (j in loopv) {
        if (independent == TRUE) {
            subset <- samples[samples %in% cmp[j, ]]
            countDFsub <- countDF[, names(subset)]
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = as.matrix(countDFsub),
                                                  colData = data.frame(condition = as.factor(subset)), design = ~condition)
            mycmp <- cmp[j, , drop = FALSE]
            if(emptyIsFalse(session)){
                updateProgressBar(
                    session, pg_id, j, cmp_len,
                    title = paste("Now comparing", glue_collapse(mycmp[1, ], " vs. ")),
                    status = if(j < cmp_len/3) "danger" else if(j < cmp_len/3*2) "warning" else "success"
                )
            }
        }
        else {
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = as.matrix(countDF),
                                                  colData = data.frame(condition = as.factor(samples)), design = ~condition)
            mycmp <- cmp
        }
        dds <- DESeq2::DESeq(dds, quiet = TRUE)
        for (i in seq(along = mycmp[, 1])) {
            if(!independent & emptyIsFalse(session)){
                updateProgressBar(
                    session, pg_id, i, cmp_len,
                    title = paste("Now comparing", glue_collapse(cmp[i, ], " vs. ")),
                    status = if(i < cmp_len/3) "danger" else if(i < cmp_len/3*2) "warning" else "success"
                )
            }
            if (lfcShrink) {
                dds$condition <- stats::relevel(dds$condition, ref = mycmp[i, 2])
                dds <- DESeq2::nbinomWaldTest(dds)
                res <- DESeq2::lfcShrink(
                    dds,
                    coef = paste0("condition_", paste0(mycmp[i, ], collapse = "_vs_")),
                    type = lfcShrink_type
                )
            } else {
                res <- DESeq2::results(dds, contrast = c("condition", mycmp[i, ]))
            }
            res[is.na(res[, "padj"]), "padj"] <- 1
            res[is.na(res[, "log2FoldChange"]), "log2FoldChange"] <- 0
            res[["pass_filter"]] <- ifelse(
                abs(res[["log2FoldChange"]]) >= as.numeric(lfc_filter) & res[["padj"]] <= as.numeric(fdr_filter)
                , 1, 0
            )
            deg_res[[paste0(mycmp[i, ], collapse = "_")]] <- res
            deg <- as.data.frame(res)
            colnames(deg)[colnames(deg) %in% c("log2FoldChange")] <- c("logFC")
            colnames(deg) <- paste(paste(mycmp[i, ], collapse = "-"),
                                   colnames(deg), sep = "_")
            deseqDF <- cbind(deseqDF, deg[rownames(deseqDF), ])
        }
    }
    return(list(
        bigtable = deseqDF,
        table_list = SummarizedExperiment::SummarizedExperiment(deg_res)
    ))
}



