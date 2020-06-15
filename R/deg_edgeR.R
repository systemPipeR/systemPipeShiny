# library(edgeR, quietly = TRUE)

deg_edgeR <- function(countDF, targets, Fold, FDR, cmpset, cmp, plot = T) {
  edgeDF <- systemPipeR::run_edgeR(countDF = countDF, targets = targets, cmp = cmp[[cmpset]], independent = FALSE, 
                      mdsplot = "")
  DEG_list <-  systemPipeR::filterDEGs(degDF = edgeDF, filter = c(Fold = Fold, FDR = FDR), plot = F)
  if (plot == T) {
    df <- DEG_list$Summary
    filter <- c(Fold = Fold, FDR = FDR)
    ## plot
    mytitle <- paste("edgeR DEG Counts (", names(filter)[1], ": ", filter[1], " & " , names(filter)[2], ": ", filter[2], "%)", sep="")
    df_plot <- data.frame(Comparisons=rep(as.character(df$Comparisons), 2), Counts=c(df$Counts_Up, df$Counts_Down), Type=rep(c("Up", "Down"), each=length(df[,1])))
    p <- ggplot(df_plot, aes(Comparisons, Counts, fill = Type)) + geom_bar(position="stack", stat="identity") + coord_flip() + theme(axis.text.y=element_text(angle=0, hjust=1)) + ggtitle(mytitle)
    ggplotly(p)
  } else {return (edgeDF)}
}
