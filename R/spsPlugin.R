##########################   SPS plugins    ####################################

spsLoadPlugin <- function(
    plugin = "",
    app_path = getwd(),
    verbose = FALSE,
    third_party = FALSE,
    overwrite = FALSE
){
    old_opt <- getOption('sps')
    spsOption("use_crayon", TRUE)
    spsOption("verbose", verbose)
    spsinfo("Check plugin name")
    if(!.validatePluginName(plugin, third_party)) return(cat())
    if(!third_party) .checkPluginInstall()
    spsinfo("Load tabs.csv from your SPS project")
    tab_info <- .checkTabFile(app_path)
    plugin_path <- if(third_party) {
        system.file("app", package = plugin)
    } else plugin
    if(!emptyIsFalse(app_path)) spserror("Can't get plugin path")
    spsinfo("Load tabs.csv from plugin")
    tab_info_plugin <- .checkTabFile(plugin_path, check_write = FALSE)
    files <- .resolvePluginStructure(app_path, plugin_path)
    if(!.resolveFileOverlap(files, overwrite)) spserror("Abort")
    .checkTabContent(tab_info_plugin)
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


.checkTabContent <- function(plugin_path){
    vs_tabs <- dplyr::filter(tab_info_plugin, type_sub %in% c("data", "plot"))
    if(nrow(vs_tabs) < 1) {spsinfo("No tab added"); return(TRUE)}
    tab_ids <- vs_tabs$tab_id; tab_files <- vs_tabs$tab_file_name
    tab_ids = c(tab_ids, "asasas"); tab_files = c(tab_files, "asasas")
    spsinfo("Checking plugin tab IDs")
    if(!all(bad_id <- str_detect(tab_ids, "^(data|plot)_"))){
        spserror(c("Invalid tab ID ",
                   glue_collapse(tab_ids[!bad_id], sep = " ")))
    }
    spsinfo("Checking plugin individual tabs")
    tab_ids = tab_ids[-1:-2]
    tab_files = tab_files[-1:-2]
    for(i in seq_along(tab_files)[-1:-2]){
        if(!str_detect(tab_files[i], glue("^tab_vs_{tab_ids[i]}\\.R$"))) {
            spswarn(c("tab ",
                      tab_files[i],
                      " recommended file name is ",
                      glue("tab_vs_{tab_ids[i]}.R")))
        }
        spsinfo(c("Checking content of ", tab_files[i]))
        tab_file <- file.path(plugin_path, "R", tab_files[i])
        if(!file.exists(tab_file)) {
            spserror(c("Plugin tab ", tab_ids[i], ": ", tab_file, " not exist"))
        }
        tab_content <- readLines(tab_file)
        .findUIandServer(tab_content, "UI", tab_ids[i], tab_file)
        .findUIandServer(tab_content, "Server", tab_ids[i], tab_file)

    }
}


.findUIandServer <- function(tab_content, ui_or_server, tab_id, tab_file){
    search_res <- str_detect(
        tab_content,
        glue("^@{tab_id}@@{ui_or_server}@[ ]{0,}(<-|=)[ ]{0,}function",
             .open = "@{",
             .close = "}@")
    )
    if(sum(search_res) < 1){
        spserror(c(glue("Cannot find {ui_or_server} function for "),
                   tab_id,
                   " in file ",
                   tab_file,
                   " It should start with:", "\n",
                   glue("'{tab_id}{ui_or_server} <- function' or \n"),
                   glue("'{tab_id}{ui_or_server} = function'\n")
        ))
    } else if(sum(search_res) > 1){
        spswarn(glue("Find duplicated {ui_or_server} function in {tab_file}:"))
        cat(glue(
            "Line @{which(search_res)}@: @{tab_content[search_res]}@",
            .open = "@{", .close = "}@"
        ), sep = "\n")
    }
}




.listPluginFiles <- function(path = getwd(), include_tab_config = FALSE){
    exclude_files <- if(!include_tab_config) "" else c("", "config/tabs.csv")
    list.files(path, full.names = FALSE, include.dirs = TRUE,
               all.files = TRUE, recursive = TRUE, no.. = TRUE) %>%
        {.[!. %in% exclude_files]}
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

.resolvePluginStructure <- function(app_path, plugin_path){
    app_files <- .listPluginFiles(app_path, TRUE)
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
    return(list(
        old_files = old_files,
        new_files = new_files,
        overlap_files = overlap_files,
        dirs = dirs
    ))
}

files = .resolvePluginStructure(".", ".")

.resolveFileOverlap <- function(files, overwrite){
    file_overlap <- files$overlap_files[!files$overlap_files %in% files$dirs]
    if(length(file_overlap) > 0 & !overwrite){
        spswarn("Overlapping files detected and `overwrite` is false")
        return(FALSE)
    }
}



