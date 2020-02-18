# library(DESeq2, quietly = TRUE)

deg_venn <- function(count_table, targets_file, Fold, FDR) {
  
  targetspath <- targets_file
  targets <- read.delim(targetspath, comment = "#")
  cmp <- readComp(file = targetspath, format = "matrix", delim = "-")
  countDFeBygpath <- count_table
  countDFeByg <- read.delim(countDFeBygpath, row.names = 1)
  degseqDF <- run_DESeq2(countDF = countDFeByg, targets = targets, cmp = cmp[[1]], 
                         independent = FALSE)
  DEG_list <- filterDEGs(degDF = degseqDF, filter = c(Fold = 2, FDR = 10), plot = F)

vennsetup <- overLapper(DEG_list$Up[6:9], type = "vennsets")
vennsetdown <- overLapper(DEG_list$Down[6:9], type = "vennsets")
vennPlot(list(vennsetup, vennsetdown), mymain = "", mysub = "", 
         colmode = 2, ccol = c("blue", "red"))
## vconfusing plot results
}
