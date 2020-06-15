## tSNE has two main parameters: the perplexity and learning rate
## tSNE is a stochastic algorithm which means running the method multiple times on the same dataset will result in different plots. 
##To ensure reproducibility, we fix the
## "seed" of the random-number generator in the code below so that we always get the same plot
## tSNE faithfully represents local relationships

# library(Rtsne)
# library(ggplot2)

run_TSNE <- function(countDF, targets) {
countDF_uni <- t(unique(countDF)) # removes duplicates and transposes matrix
set.seed(42)
tsne_out <- Rtsne(countDF_uni,dims = 2,pca=T,perplexity=5,theta=0.0) # Run TSNE
targets <- data.frame(targets)
Sample <- targets$Factor
plotdata <- data.frame(tsne_x = tsne_out$Y[,1], tsne_y = tsne_out$Y[,2])
g <- ggplot(plotdata, aes(x = tsne_x, y = tsne_y, color = Sample)) + geom_point(size =2) + ggtitle("t-SNE")
ggplotly(g)


# testing:
# plot(tsne_out$Y,col=colors, asp=1, pch = 16)
# legend("topleft", legend = targets$SampleName, cex = 0.5, col = c("blue","red"))
# plot(tsne_out$Y, t='n', main="tsne")
# text(tsne_out$Y, labels=targets$SampleName, col=colors[targets$SampleName])


}
