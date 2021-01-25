# only load things if you are working in Rstudio

if(interactive() & Sys.getenv("RSTUDIO", "") == "1"){
    cat("load package funcs\n")
    devtools::load_all()
    cat("check if test folder exists\n")
    dirs = list.dirs(recursive = FALSE)
    dirs = grep(x = dirs, pattern = "SPS_20201001", value = TRUE)
    cat("setup done\n")
    if(length(dirs) == 1){
        cat("To use the test dir, use Ctrl+shift+h or run:\n")
        cat(paste0("setwd('", normalizePath(dirs), "')\n"))
    }
}


