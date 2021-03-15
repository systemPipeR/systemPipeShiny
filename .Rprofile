# only load things if you are working in Rstudio

if(interactive() & Sys.getenv("RSTUDIO", "") == "1"){
    (function(){
        if (!requireNamespace("devtools", quietly = TRUE)) {
            return(cat("Install {devtools} to develop this package\n"))
        }
        cat("Load package funcs...\n")
        devtools::load_all()
    })()

    cat("Check if test folder exists...\n")
    dirs = list.dirs(recursive = FALSE)
    dirs = grep(x = dirs, pattern = "SPS_20", value = TRUE)
    if(length(dirs) > 0){
        cat("To use the test dir, use Ctrl+shift+h or run:\n")
        cat(paste0("setwd('", normalizePath(dirs[1]), "')\n"))
    } else {
        cat("For developers, you may want to create a test instance by `systemPipeShiny::spsInit()`\n")
    }
} else if (interactive()) {
    cat("It is recommended to use Rstudio to develop this package.\n")
}


