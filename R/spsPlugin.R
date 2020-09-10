##########################   SPS plugins    ####################################

spsLoadPlugin <- function(
    plugin = "",
    app_path = getwd(),
    verbose = FALSE,
    third_party = FALSE,
    overwrite = FALSE,
    colorful = TRUE
){
    old_opt <- getOption('sps')
    spsOption("use_crayon", colorful)
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
    if(!.checkConflictTabs(tab_info, tab_info_plugin)) spserror("Abort")
    files <- .resolvePluginStructure(app_path, plugin_path)
    if(!.resolveFileOverlap(files, overwrite)) spserror("Abort")
    .checkTabContent(tab_info_plugin)
    spsinfo("Initial checks pass, now copy files")
    .copyPluginFiles(files, app_path, plugin_path, tab_info, tab_info_plugin)
    options(sps = old_opt)
    msg(gue("Plugin {plugin} added!"), "SPS-INFO", "green")
}


spsRemovePlugin <- function(
    plugin,
    app_path = getwd(),
){
    old_opt <- getOption('sps')
    spsOption("use_crayon", TRUE)
    spsOption("verbose", verbose)
    if(!is.character(plugin) & length(plugin) != 1){
        spserror("Plugin name must be a length 1 character string")
    }
    options(sps = old_opt)
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

.listPluginFiles <- function(path = getwd(), exclude_tab_config = FALSE){
    exclude_files <- if(!exclude_tab_config) "" else c("", "config/tabs.csv")
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
    app_files <- .listPluginFiles(app_path)
    plugin_files <- .listPluginFiles(plugin_path, TRUE)
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
    for(dir in dirs){
        color_text[all_files == dir]<- crayon::blue$bold(basename(dir))
    }
    names(all_files) <- color_text
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

files = .resolvePluginStructure("SPS_20200909", "test")

.resolveFileOverlap <- function(files, overwrite){
    file_overlap <- files$overlap_files[!files$overlap_files %in% files$dirs]
    if(length(file_overlap) > 0 & !overwrite){
        spswarn("Overlapping files detected and `overwrite` option is false")
        FALSE
    } else {
        spswarn("Overlapping files detected and will be overwritten")
        TRUE
    }
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
    spsinfo("Checking plugin individual tab files")
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

.checkConflictTabs <- function(tab_info, tab_info_plugin){
    conflict <- intersect(tab_info$tab_id, tab_info_plugin$tab_id)
    if(length(conflict) > 0){
        spswarn(c("Tab ID(s) '",
                  glue_collapse(conflict, ", "),
                  "' have conflict"))
        FALSE
    } else TRUE
}

.copyPluginFiles <- function(files, app_path, plugin_path,
                             tab_info, tab_info_plugin){
    copy_files <- c(files$new_files, files$overlap_files) %>% {
        .[!. %in% files$dirs]
    }
    spsinfo("create required directories")
    dir_result <- lapply(file.path(app_path, files$dirs),
                         dir.create,
                         recursive = TRUE,
                         showWarnings = FALSE) %>%
        unlist()
    lapply(files$dirs[!dir_result], function(x) {
        spsinfo(glue("Directory {x} exists, skip"))
    })
    copy_res <- file.copy(file.path(plugin_path, copy_files),
                          file.path(app_path, copy_files),
                          overwrite = TRUE)
    if(!all(copy_res)){
        lapply(file.path(app_path, copy_files), function(x){
            spswarn(glue("Cannot copy file to {x}"))
            spsinfo(c("Maybe this file(s) exists but ",
                      "you have no permission to modify"), TRUE)
            spserror("Abort")
        })
    }
    spsinfo("Now rewrite 'config/tabs.csv'")
    tab_info_new <- tab_info %>% dplyr::add_row(tab_info_plugin)
    header <- readLines(file.path(app_path, "config", "tabs.csv")) %>%
        {.[str_which(., "^#")]}
    c(header, names(tab_info_new) %>% glue_collapse(sep = ","),
      apply(tab_info_new, 1, paste, collapse = ",")) %>%
        writeLines(file.path(app_path, "config", "tabs.csv"))
}






