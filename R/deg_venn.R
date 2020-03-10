
deg_venn <- function(DF, FDR, Fold, cmp_list) {
DEG_list <- systemPipeR::filterDEGs(degDF = DF, filter = c(Fold = Fold, FDR = FDR), plot = F)
up <- DEG_list$Up
down <- DEG_list$Down
index.up <- which(names(DEG_list$Up) %in% cmp_list)
index.down <- which(names(DEG_list$Down) %in% cmp_list)
listup <- DEG_list$Up[index.up]
listdown <- DEG_list$Down[index.down]
vennsetup <- systemPipeR::overLapper(listup, type = "vennsets")
vennsetdown <- systemPipeR::overLapper(listdown, type = "vennsets")
systemPipeR::vennPlot(list(vennsetup, vennsetdown), mymain = "", mysub = "", 
         colmode = 2, ccol = c("blue", "red"))
}
