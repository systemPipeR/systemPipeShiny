##########################   SPS plugins    ####################################

#' SPS plugin operations
#' @description `spsLoadPlugin()` is used to load an existing SPS plugin,
#' `spsRemovePlugin()` is to remove a loaded plugin, and `spsNewPlugin()` is
#' for developers to create a minimum plugin structure and required files.
#' @param plugin character, a plugin name. It can also be a path in
#' `spsLoadPlugin` function  when if `third_party = TRUE`.
#' @param app_path the SPS project you want to load plugin to
#' @param verbose bool, show more information?
#' @param third_party bool, is this an official plugin?
#' @param overwrite bool, if there are file conflicts, overwrite current
#' existing files in `app_path`?
#' @param colorful bool, colorful message?
#'
#' @return No return
#' @details
#' ### General
#'
#' - Make sure there is a 'config/tabs.csv' file in your SPS project when you
#' load the plugin.
#' - You can just use `spsLoadPlugin()` without any argument to see
#' what are the plugin options.
#' - If there is any file conflicts when loading plugins, please see the
#' app structure tree and refer to the legend below the tree to help you resolve
#' conflicts. Overwriting current files is not recommended. Rename conflict
#' files and compare them after loading the plugin will be better.
#' - When a plugin is removed, only tab files from that plugin are removed and
#' entries on *config/tabs.csv* are removed. Other files come from the plugin
#' will not be removed.
#'
#' ### Building a plugin
#' - When adding new tabs to a plugin, it will be better to set working
#' directory to *PLUGIN_ROOT/inst/app*. Tab files should go into
#' *PLUGIN_ROOT/inst/app/R*.
#' - Any additional files in *PLUGIN_ROOT/inst/app* except the *config/tabs.csv*
#' will also be copied to users' project.
#' - Tab files will be checked and two functions in the tab file are expected:
#' `tabIDUI` and `tabIDServer`. Other files will not be checked for content.
#'
#' @export
#'
#' @examples
#' # see what official plugins you can install:
#' spsLoadPlugin()
#' # create a project
#' spsInit(project_name = "testProject",
#'         change_wd = FALSE,
#'         open_files = TRUE,
#'         overwrite = TRUE)
#' # create a new plugin
#' spsNewPlugin(path = "testPlugin")
#' # add some tabs to the plugin
#' # tabs
#' plugin_path <- file.path("testPlugin", "inst", "app")
#' newTabData("data_a",
#'            app_path = plugin_path,
#'            plugin = "testPlugin",
#'            reformat = FALSE,
#'            open_file = FALSE)
#' newTabPlot("plot_a",
#'            app_path = plugin_path,
#'            plot_data = list(makePlotData(receive_datatab_ids = 'data_a',
#'                                     app_path = plugin_path)),
#'            plugin = "testPlugin",
#'            reformat = FALSE,
#'            open_file = FALSE)
#' # load the plugin, the plugin is not published, so `third_party = TRUE`
#' spsLoadPlugin(plugin = "testPlugin",
#'               app_path = "testProject",
#'               third_party = TRUE,
#'               overwrite = FALSE)
#' # check if the plugin is added
#' # You should see `tab_vs_data_a.R` and `tab_vs_plot_a.R`
#' list.files(file.path("testProject", "R"))
#' # check if tab files are registered
#' # You should see the two new records
#' vroom::vroom(file.path("testProject", "config", "tabs.csv"),
#'              comment = "#") %>% tail()
#' # now remove the plugin
#' spsRemovePlugin(plugin = "testPlugin", app_path = "testProject", force = TRUE)
#' # let check these files again:
#' list.files(file.path("testProject", "R"))
#' vroom::vroom(file.path("testProject", "config", "tabs.csv"),
#'              comment = "#") %>% tail()
spsLoadPlugin <- function(
    plugin = "",
    app_path = getwd(),
    verbose = FALSE,
    third_party = FALSE,
    overwrite = FALSE,
    colorful = TRUE){
    old_opt <- getOption('sps')
    on.exit(options(sps = old_opt))
    spsOption("use_crayon", colorful)
    spsOption("verbose", verbose)
    spsinfo("Check plugin name")
    if(!.validatePluginName(plugin, third_party)) return(cat())
    if(!third_party) .checkPluginInstall()
    spsinfo("Load tabs.csv from your SPS project")
    tab_info <- .checkTabFile(app_path)
    plugin_path <- if(!third_party) {
        system.file("app", package = plugin)
    } else file.path(plugin, "inst", "app")
    if(!emptyIsFalse(app_path)) spserror("Can't get plugin path")
    spsinfo("Load tabs.csv from plugin")
    tab_info_plugin <- .checkTabFile(plugin_path, check_write = FALSE)
    if(!.checkConflictTabs(tab_info, tab_info_plugin)) spserror("Abort")
    files <- .resolvePluginStructure(app_path, plugin_path)
    if(!.resolveFileOverlap(files, overwrite)) spserror("Abort")
    .checkTabContent(tab_info_plugin)
    spsinfo("Initial checks pass, now copy files")
    .copyPluginFiles(files, app_path, plugin_path, tab_info, tab_info_plugin)
    msg(glue("Plugin {plugin} added!"), "SPS-SUCCESS", "green")
    more_info <- c(glue("Remember to load `library({plugin})` when you start "),
                   "SPS. Additional R functions maybe bundled with this plugin")
    if(!third_party) {spsinfo(more_info, TRUE)} else {
        spsinfo(glue("If this third-party plugin is an R package "), TRUE)
        spsinfo(more_info, TRUE)
    }
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
        R_folder <- file.path(plugin, "inst", "app", "R")
        if(!dir.exists(R_folder))
            spserror(glue("{R_folder} does not exist"))
        tab_file <- file.path(plugin, "inst", "app", "config", "tabs.csv")
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

#' @importFrom crayon blue green
#' @noRd
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

.checkTabFile <- function(project_path, check_write = TRUE){
    if(check_write){
        spsinfo("Check if app folder is writeable")
        if(!is.writeable(project_path)){
            spserror(glue('App folder {project_path} is not writeable'))
        }
    }
    spsinfo("Check if tabs.csv exists")
    tab_file <- file.path(project_path, "config", "tabs.csv")
    if(!file.exists(tab_file)){
        spserror(glue('Expect the tabs.csv file at: {tab_file}'))
    }
    spsinfo("Check tabs.csv content")
    return(suppressWarnings(checkTabs(project_path)))
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

#' @importFrom crayon make_style green blue
#' @noRd
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


.resolveFileOverlap <- function(files, overwrite){
    file_overlap <- files$overlap_files[!files$overlap_files %in% files$dirs]
    if(length(file_overlap) > 0 & !overwrite){
        spswarn("Overlapping files detected and `overwrite` option is false")
        FALSE
    } else if(length(file_overlap) > 0 & overwrite){
        spswarn("Overlapping files detected and will be overwritten")
        TRUE
    } else TRUE
}


#' @importFrom dplyr filter
#' @noRd
.checkTabContent <- function(tab_info_plugin){
    vs_tabs <- dplyr::filter(tab_info_plugin, type_sub %in% c("data", "plot"))
    if(nrow(vs_tabs) < 1) {spsinfo("No tab added"); return(TRUE)}
    tab_ids <- vs_tabs$tab_id; tab_files <- vs_tabs$tab_file_name
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
        spswarn(c("There is(are) Tab ID(s) from plugin that conflicts with",
                  " your current SPS project:\n '",
                  glue_collapse(unique(conflict), ", "), "'"))
        FALSE
    } else TRUE
}

#' @importFrom dplyr add_row
#' @noRd
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

           ########### remove plugin #############

#' @rdname spsLoadPlugin
#' @param force bool, if plugin files found, confirm before remove?
#' @export
#' @importFrom dplyr filter
spsRemovePlugin <- function(
    plugin="",
    app_path = getwd(),
    force = FALSE,
    verbose = FALSE,
    colorful = TRUE){
    old_opt <- getOption('sps')
    spsOption("use_crayon", colorful)
    spsOption("verbose", verbose)
    if(!is.character(plugin) & length(plugin) != 1){
        spserror("Plugin name must be a length 1 character string")
    }
    spsinfo("Load tabs.csv from your SPS project")
    tab_info <- .checkTabFile(app_path)
    tab_plugin <- dplyr::filter(tab_info, plugin != "core")
    if(plugin == ""){
        spsinfo("Installed plugins:", TRUE)
        cat("core(unremovable)\n")
        return(cat(unique(tab_plugin$plugin), sep = "\n"))
    }
    if(!plugin %in% tab_plugin$plugin) return(spswarn("No plugin matched"))
    files <- tab_plugin$tab_file_name[tab_plugin$plugin == plugin]
    if(all(.removePluginFiles(files, app_path, force))) {
        spsinfo("Files all removed")}
    spsinfo("Now rewrite 'config/tabs.csv'")
    header <- readLines(file.path(app_path, "config", "tabs.csv")) %>%
        {.[str_which(., "^#")]}
    c(header, names(tab_info) %>% glue_collapse(sep = ","),
      apply(tab_info[tab_info$plugin != plugin,], 1, paste, collapse = ",")) %>%
        writeLines(file.path(app_path, "config", "tabs.csv"))
    options(sps = old_opt)
    msg(glue("Plugin {plugin} removed!"), "SPS-SUCCESS", "green")
}

.removePluginFiles <- function(files, app_path, force){
    glue_collapse(files, sep = ", ") %>% {
        spsinfo(glue("Matched plugin tab files(s): {.}"), TRUE)
    }
    if(!force){
        switch(menu(c("YES", "NO"), title = "Continue?"),
               {},
               return(spserror("Abort"))
        )
    }
    spsinfo(glue("Now remove files"))
    shinyCatch(file.remove(file.path(app_path, "R", files)), shiny = FALSE)
}

########### new plugin #############
#' @rdname spsLoadPlugin
#' @param path character string, path of where you want to create the plugin
#' directory, can be a non-existing location but make sure you have write
#' permission.
#' @param readme bool, created *README.md* file?
#' @export
spsNewPlugin <- function(path, readme = TRUE, verbose = FALSE, colorful = TRUE){
    old_opt <- getOption('sps')
    on.exit(options(sps = old_opt))
    spsOption("use_crayon", colorful)
    spsOption("verbose", verbose)
    if(length(path) != 1 | !is.character(path)) {
        spserror("Path must be a length 1 character string")}
    if(str_detect(basename(path), "([[:punct:]]|\\s)")) {
        spserror("Special character or space is not allowed for SPS plugin name")
    }
    if(!dir.exists(path)){
        spsinfo(c(path, "does not exist, try to create"), TRUE)
        if(!shinyCatch(dir.create(path, recursive = TRUE), shiny = F)){
            spserror("Cannot create directory, abort")
        }
    } else if(!is.writeable(path)) spserror("Path exists but not writeable")
    app_path <- system.file(package = "systemPipeShiny","app")
    if(!emptyIsFalse(app_path)){
        spserror("Cannot find tab file in SPS package, installation problem")
    }
    tab_head <- readLines(file.path(app_path, "config", "tabs.csv")) %>%
        {.[str_which(., "^#")]}
    col_names <- suppressMessages(.checkTabFile(app_path)) %>%
        names() %>%
        glue_collapse(sep = ",")
    dirs = c("R", "data", "config") %>% file.path(path, "inst", "app", .)
    spsinfo("Create directories")
    lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
    spsinfo("Write tab file")
    writeLines(c(tab_head, col_names),
               file.path(dirs[3], "tabs.csv"))
    if(readme){
        spsinfo("write read me")
        writeLines(
            c(glue("# {basename(path)}\n\n"),
              glue("{basename(path)} is a systemPipeShiny plugin.")),
            file.path(path, "README.md")
        )
    }
    msg(glue("Plugin {path} created"), "SPS-SUCCESS", "green")
    spsinfo(c("Optional: To format the plugin ",
              "folder to a more publishable structure, run following"),
            TRUE)
    cat(glue('usethis::create_package("{path}")'))
}
