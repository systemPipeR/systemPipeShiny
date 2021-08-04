# only load things if you are working in Rstudio

if(interactive() && Sys.getenv("RSTUDIO", "") == "1"){
    (function(){
        if (!requireNamespace("devtools", quietly = TRUE)) {
            return(cat("Install {devtools} to develop this package\n. `install.packages('devtools')`"))
        }
        cat("Load package funcs...\n")
        devtools::load_all()
        cat("Check if test folder exists...\n")
        .dirs = list.dirs(recursive = FALSE)
        .dirs_sps = grep(x = .dirs, pattern = "SPS_20", value = TRUE)
        if(length(.dirs_sps)){
            cat("To use the project test dir, use Ctrl+shift+h or run:\n")
            cat(paste0("setwd('", normalizePath(.dirs_sps[1]), "')\n"))
        } else {
            cat("For developers, you may want to create a test SPS instance by `systemPipeShiny::spsInit()`\n")
        }
        .dirs_test = grep(x = .dirs, pattern = "test$", value = TRUE)
        if(length(.dirs_test)) {
            cat("Random test directory is detected, use Ctrl+shift+h or run:\n")
            cat(paste0("setwd('", normalizePath(.dirs_test[1]), "')\n"))
        }
    })()
} else if (interactive()) {
    cat("You are in SPS develop environment. It is recommended to use Rstudio to develop this package.\n")
}


