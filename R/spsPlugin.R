##########################   SPS plugins    ####################################

spsLoadPlugin <- function(
    plugin = "",
    app_path = getwd(),
    verbose = FALSE,
    third_party = FALSE,
    confirm = TRUE,
    overwrite = FALSE
){
    old_opt <- getOption('sps')
    spsOption("use_crayon", TRUE)
    spsOption("verbose", verbose)
    spsinfo("Check plugin name")
    if(!.validatePluginName(plugin, third_party)){
        return(cat())
    }
    if(!third_party) .checkPluginInstall()
    spsinfo("Load tabs.csv from your SPS project")
    tab_info <- .checkTabFile(app_path)
    plugin_path <- if(third_party) {
        system.file("app", package = plugin)
    } else plugin
    if(!emptyIsFalse(app_path)) spserror("Can't get plugin path")
    spsinfo("Load tabs.csv from plugin")
    tab_info_plugin <- .checkTabFile(plugin_path, check_write = FALSE)
    .resolvePluginStructure
    options(sps = old_opt)
}

spsLoadPlugin("base", verbose = TRUE)

Windows
spsRemovePlugin <- function(

){

}



.checkPluginInstall <- function(plugin){
    spsinfo("Check if plugin(package) is installed")
    if(emptyIsFalse(checkNameSpace(plugin, quietly = TRUE))){
        on.exit(.listPlugins())
        spserror(glue("Plugin `{plugin}` is not installed"))
    }
}
.validatePluginName <- function(plugin, third_party){
    if(length(plugin) > 1) spserror("Only one name at a time")
    if(third_party){
        R_folder <- file.path(plugin, "R")
        if(!dir.exists(R_folder))
            spserror(glue("{R_folder} does not exist"))
        tab_file <- file.path(plugin, "config", "tabs.csv")
        if(!file.exists(tab_file))
            spserror(glue("{tab_file} does not exist"))
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

.checkTabFile <- function(app_path, check_write = TRUE){
    if(check_write){
        spsinfo("Check if app folder is writeable")
        if(!is.writeable(app_path)){
            spserror(glue('App folder {app_path} is not writeable'))
        }
    }
    spsinfo("Check if tabs.csv exists")
    tab_file <- file.path(app_path, "config", "tabs.csv")
    if(!file.exists(tab_file)){
        spserror(glue('Expect the tabs.csv file at: {tab_file}'))
    }
    spsinfo("Check tabs.csv content")
    return(suppressWarnings(checkTabs(app_path)))
}

.resolvePluginStructure <- function(app_path, plugin_path){
    app_files <- .listPluginFiles(app_path)
    plugin_files <- .listPluginFiles(plugin_path)
    all_files <- unique(app_files, plugin_files)

    old_files <- setdiff(app_files, plugin_files)
    new_files <- setdiff(plugin_files, app_files)
    overlap_files <- all_files[!all_files %in% c(old_files, new_files)]
    names(old_files) <- basename(old_files)
    names(new_files) <- if(emptyIsFalse(new_files)){
        crayon::green$bold(paste0("*", basename(new_files)))
    } else {character(0)}
    names(overlap_files) <- if(emptyIsFalse(overlap_files)){
        crayon::make_style("orange")$bold(paste0("**", basename(overlap_files)))
    } else(character(0))

    all_files <- c(old_files, new_files, overlap_files)
    dirs <- unique(dirname(all_files)) %>% {.[. != "."]}
    color_text <- names(all_files)
    color_text[all_files %in% dirs] <- crayon::blue$bold(basename(dirs))
    names(all_files) <-color_text
    split_files <- split(all_files, dirname(all_files))

    cat(crayon::blue$bold(basename("New directory will be:")), "\n")
    .cat_dir(split_files)
    cat("-- Old files ",
        crayon::green$bold("-- *New files"), "\n",
        crayon::make_style("orange")$bold("-- **Overlapping files"), " ",
        crayon::blue$bold("Directory"), "\n", sep = ""
    )
}

.resolvePluginStructure(".", ".")

.listPluginFiles <- function(path = getwd()){
    list.files(path, full.names = FALSE, include.dirs = TRUE,
               all.files = TRUE, recursive = TRUE, no.. = TRUE) %>%
        {.[. != ""]}
}

.cat_dir <- function(split_files, start = ".", space = "") {
    nodes <- split_files[[start]]
    color_text <- names(nodes)
    for(dir in seq_along(nodes)) {
        if(dir == length(nodes)) {
            cat(space, "L-- ", color_text[dir], "\n", sep = "")
            .cat_dir(split_files, nodes[dir], glue("{space}    "))
        } else {
            cat(space, "+-- ", color_text[dir], "\n", sep = "")
            .cat_dir(split_files, nodes[dir], glue("{space}|   "))
        }
    }
}





