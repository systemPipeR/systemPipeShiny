# library(glmpca)

run_GLM <- function(countDF, targets, colData) {
  ## Create full DESeqDataSet object
  dds <- DESeqDataSetFromMatrix(countData = countDF, colData = colData, 
                                design = ~condition)
  count_mat <- counts(dds)
  nozero <- count_mat[which(rowSums(count_mat) > 0),]  
  gpca <- glmpca(nozero, L=2)
  gpca.dat <- gpca$factors
  gpca.dat$condition <- dds$condition
  Sample <- targets$Factor
  g <- ggplot(gpca.dat, aes(x = dim1, y = dim2, color = Sample)) +   geom_point(size =2) + coord_fixed() + ggtitle("Generalized PCA (GLM-PCA)")
  ggplotly(g)
  
}
