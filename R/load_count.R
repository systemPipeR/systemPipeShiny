# library(systemPipeR)
load_count <- function(count_table, targets_file) {
if (!(is.null(targets_file)) == T) {
  targets <- read.delim(targets_file, comment.char = "#")
  targets <- data.frame(targets)
  colData <- data.frame(row.names = targets$SampleName, 
                        condition = targets$Factor)
}
countDF <- as.matrix(read.table(count_table))
cmp <- readComp(file = targets_file, format = "matrix", delim = "-")
list <- list(cmp = cmp, countDF = countDF, targets = targets, colData = colData)
return(list)
}

##load_count("counttable.xls", "targets.txt")
readComp <- function (file, format = "vector", delim = "-") 
{
    if (!format %in% c("vector", "matrix")) 
        stop("Argument format can only be assigned: vector or matrix!")
    if (class(file) == "SYSargs") {
        if (length(targetsheader(file)) == 0) 
            stop("Input has no targets header lines.")
        comp <- targetsheader(file)
    }
    else if (class(file) == "SYSargs2") {
        if (length(targetsheader(file)[[1]]) == 0) 
            stop("Input has no targets header lines.")
        comp <- targetsheader(file)[[1]]
    }
    else {
        comp <- readLines(file)
    }
    comp <- comp[grepl("<CMP>", comp)]
    comp <- gsub("#.*<CMP>| {1,}", "", comp)
    comp <- gsub("\t", "", comp)
    comp <- gsub("^\"|\"$", "", comp)
    comp <- strsplit(comp, ":|,")
    names(comp) <- lapply(seq(along = comp), function(x) comp[[x]][1])
    comp <- sapply(names(comp), function(x) comp[[x]][-1], simplify = FALSE)
    checkvalues <- unique(unlist(strsplit(unlist(comp), "-")))
    checkvalues <- checkvalues[checkvalues != "ALL"]
    if (class(file) == "SYSargs") {
        all <- unique(as.character(targetsin(file)$Factor))
    }
    else if (class(file) == "SYSargs2") {
        all <- unique(as.character(targets.as.df(targets(args_bam))$Factor))
    }
    else {
        all <- unique(as.character(read.delim(file, comment.char = "#")$Factor))
    }
    if (any(!checkvalues %in% all)) 
        stop(paste("The following samples are not present in Factor column of targets file:", 
                   paste(checkvalues[!checkvalues %in% all], collapse = ", ")))
    allindex <- sapply(names(comp), function(x) any(grepl("ALL", 
                                                          comp[[x]])))
    if (any(allindex)) 
        for (i in which(allindex)) comp[[i]] <- combn(all, m = 2, 
                                                      FUN = paste, collapse = delim)
    if (format == "vector" & delim != "-") 
        comp <- sapply(names(comp), function(x) gsub("-", 
                                                     delim, comp[[x]]), simplify = FALSE)
    if (format == "vector") 
        return(comp)
    if (format == "matrix") 
        return(sapply(names(comp), function(x) do.call("rbind", 
                                                       strsplit(comp[[x]], "-")), simplify = FALSE))
}