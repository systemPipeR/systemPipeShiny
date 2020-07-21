##takes DEG DFs from deseq2, edgeR, and limma and finds the number of up and down regulated genes that overlap

venn_methods <- function(deseqDF, edgeDF, limmaDF) {
  
DEGdeseq <- filterDEGs(degDF = deseqDF, filter = c(Fold = 2, FDR = 10))
edge_list2 <- filterDEGs(degDF = edgeDF, filter = c(Fold = 2, FDR = 10))
limmafilter <- filterDEGs(degDF = limmaDF, filter = c(Fold = 2, FDR = 10))

upregdeseq = NULL
for (i in seq(along=DEGdeseq$Up)){
  upregdeseq <- c(upregdeseq, DEGdeseq$Up[[i]])
}

upregedge= NULL
for (i in seq(along=edge_list2$Up)){
  upregedge <- c(upregedge, edge_list2$Up[[i]])
}

upreglimma= NULL
for (i in seq(along=limmafilter$Up)){
  upreglimma <- c(upreglimma, limmafilter$Up[[i]])
}

downregdeseq = NULL
for (i in seq(along=DEGdeseq$Down)){
  downregdeseq <- c(downregdeseq, DEGdeseq$Down[[i]])
}

downregedge= NULL
for (i in seq(along=edge_list2$Down)){
  downregedge <- c(downregedge, edge_list2$Down[[i]])
}

downreglimma= NULL
for (i in seq(along=limmafilter$Down)){
  downreglimma <- c(downreglimma, limmafilter$Down[[i]])
}

countUp <- list("deseq2" = upregdeseq, "edge" = upregedge, "limma" = upreglimma)
countDown <- list("deseq2" = downregdeseq, "edge" = downregedge, "limma" = downreglimma)
vennsetup <- overLapper(countUp, type = "vennsets")
vennsetdown <- overLapper(countDown, type = "vennsets")
vennPlot(list(vennsetup, vennsetdown), mymain = "", mysub = "", colmode = 2, ccol = c("blue", 
                                                                                      "red"))
}

