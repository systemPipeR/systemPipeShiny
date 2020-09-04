##########################   SPS plugins    ####################################

spsLoadPlugin <- function(
    plugin = "",
    app_dir = getwd(),
    verbose = FALSE,
    third_party = FALSE
){
    old_opt <- getOption('sps')
    spsOption("use_crayon", TRUE)
    spsOption("verbose", verbose)
    spsinfo("Check plugin name")
    if(!.validatePluginName(plugin, third_party)){
        return(cat())
    }
    spsinfo("Check if plugin(package) is installed")
    if(emptyIsFalse(checkNameSpace(plugin, quietly = TRUE))){
        on.exit(.listPlugins())
        spserror(glue("Plugin `{plugin}` is not installed"))
    }
    tab_info <- .checkTabFile(app_dir)
    options(sps = old_opt)
}

spsLoadPlugin("base", verbose = TRUE)


spsRemovePlugin <- function(

){

}

.validatePluginName <- function(plugin, third_party){
    if(length(plugin) > 1) spserror("Only one name at a time")
    if(third_party){
        if(!dir.exists(plugin) spserror(glue("{} does not exist"))

        return(TRUE)
    }
    if(!emptyIsFalse(plugin)){
        .listPlugins()
        return(FALSE)
    } else if(!plugin %in% c("spsBio", "spsDS", "base")) {
        on.exit(.listPlugins())
        spserror("Invalid plugin name")
    } else {return(TRUE)}
}

.listPlugins <- function(){
    spsinfo(glue('Current SPS has following plugins'), TRUE)
    style <- function(plugin, desc){
        cat(crayon::blue$bold(str_pad(plugin, width = 20, side = "right")),
            crayon::green$bold(desc))
        cat("\n")
    }

    style("spsBio", "Biological plots")
    cat('BiocManager::install("systemPipeR/spsBio")\n')
    style("spsDS", "A variety of plots for general data analysis")
    cat('remotes::install_github("systemPipeR/spsDS")\n')
}

.checkTabFile <- function(app_dir){
    spsinfo("Check if app folder is writeable")
    if(!is.writeable(app_dir)){
        spserror(glue('App folder {app_dir} is not writeable'))
    }
    spsinfo("Check if tabs.csv exists")
    tab_file <- file.path(app_dir, "config", "tabs.csv")
    if(!file.exists(tab_file)){
        spserror(glue('Expect the tabs.csv file at: {tab_file}'))
    }
    spsinfo("Check tabs.csv content")
    return(suppressWarnings(checkTabs(app_dir)))
}

.checkTabFile(getwd())
