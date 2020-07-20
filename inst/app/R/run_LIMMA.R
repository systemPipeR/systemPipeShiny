# library(limma)

run_LIMMA <- function(count_table, targetspath, approach, independent=TRUE, paired=NULL, mdsplot = "") {
  targets <- read.delim(targetspath, comment = "#")
  cmp <- readComp(file = targetspath, format = "matrix", delim = "-")
  cmp = cmp[[1]]
  
  countDFeBygpath <- count_table
  countDFeByg <- read.delim(countDFeBygpath, row.names = 1)
  countDF <- countDFeByg
  
  if(class(cmp) != "matrix" & length(cmp)==2) cmp <- t(as.matrix(cmp)) # If cmp is vector of length 2, convert it to matrix.
  samples <- as.character(targets$Factor); names(samples) <- paste(as.character(targets$SampleName), "", sep="")
  countDF <- countDF[, names(samples)]
  countDF[is.na(countDF)] <- 0
  DF <- data.frame(row.names=rownames(countDF)) 
  group <- as.character(samples)
  
  if(independent==TRUE) {
    loopv <- seq(along=cmp[,1])
  } else {
    loopv <- 1
  }

  for(j in loopv) {
    ## Filtering and normalization
    y <- DGEList(counts=countDF, group=group) # Constructs DGEList object
    if(independent == TRUE) {
      subset <- samples[samples %in% cmp[j,]]
      y <- y[, names(subset)]
      y$samples$group <- factor(as.character(y$samples$group))
    }
    keep <- rowSums(cpm(y)>1) >= 2; y <- y[keep, ]
    y <- calcNormFactors(y)
    ## Design matrix
    if(length(paired)==0) {
      design <- model.matrix(~0+y$samples$group, data=y$samples)
      colnames(design) <- levels(y$samples$group)
    } else {
      if(length(paired)>0 & independent==FALSE) stop("When providing values under 'paired' also set independent=TRUE")
      Subject <- factor(paired[samples %in% cmp[j,]]) # corrected Jun 2014 (won't change results)
      Treat <- y$samples$group
      design <- model.matrix(~Subject+Treat)
      levels(design) <- levels(y$samples$group)
    }
  
    if(independent == TRUE) {
      mycomp <- paste(cmp[j,1], cmp[j,2], sep="-")
    } else {
      mycomp <- paste(cmp[,1], cmp[,2], sep="-")
    }
    if(length(paired)==0) contrasts <- makeContrasts(contrasts=mycomp, levels=design)
    
    
    if(approach == "limma-trend") { ## prior trend approach 
      for(i in seq(along=mycomp)) {
        # deg <- as.data.frame(topTags(lrt, n=length(rownames(y))))
        logCPM <- cpm(y, log=TRUE, prior.count=3) ## counts are converted to logCPM values
        fit <- lmFit(logCPM, design)
        fit <- eBayes(fit, trend=TRUE)
        upordown <- summary(decideTests(fit))
        ## all genes, p-value adjusted by fdr
        deg <- as.data.frame(topTable(fit, coef=ncol(design), number = length(rownames(y)), adjust = "fdr"), n=length(rownames(y)))
        colnames(deg) <- c("logFC", "AveExpr", "t", "pvalue", "FDR", "B")
        colnames(deg) <- paste(paste(mycomp[i], collapse="_"), colnames(deg), sep="_")
        DF <- cbind(DF, deg[rownames(DF),]) 
      }
      summary(decideTests(fit))
      
    }
    
    if(approach == "voom") { ## precision weights approach
      for(i in seq(along=mycomp)) {
      v <- voom(y, design, plot=TRUE) ## voom transformation
      fit <- lmFit(v, design)
      fit <- eBayes(fit)
      deg <- as.data.frame(topTable(fit, coef=ncol(design), number = length(rownames(y))), n=length(rownames(y)))
      colnames(deg) <- c("logFC", "AveExpr", "t", "pvalue", "FDR", "B")
      colnames(deg) <- paste(paste(mycomp[i], collapse="_"), colnames(deg), sep="_")
      DF <- cbind(DF, deg[rownames(DF),]) 
      
      }
    }
 
  }
  return(DF)
  
  ##example: run_LIMMA("countDFeByg.xls","targets.txt","limma-trend")
  ##or run_LIMMA("countDFeByg.xls","targets.txt","voom")
}
