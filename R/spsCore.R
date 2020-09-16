######################### SPS main function #############################
# Initiation, creating tabs etc.


#' @import shiny assertthat stringr magrittr glue ggplot2 shinyTree
NULL


#' SystemPipeShiny app main function
#' @param vstabs custom visualization tab IDs that you want to display, in a character
#' vector. Use [spsTabInfo()] to see what tab IDs you can load
#' @param plugin If you have loaded some SPS plugins by [spsAddPlugin()],
#' you can specify here as a character vector, and it will load all tabs that
#' belong to that plugin to SPS. If you only want
#' certain tabs from a plugin, specify in `vstabs` argument.
#' @param server_expr additional top level sever expression you want
#' to run. This will run after the default server expressions. It means you can
#' have access to internal server expression objects, like the
#' [shiny::reactiveValues()]
#' object `shared`. You can also overwrite other values. Read "shared object" in
#' vignette.
#' @param app_path SPS project path
#' @details You must set the project root as working directory for this
#' function to find required files.
#' @importFrom dplyr as_tibble filter tibble
#' @importFrom rlang enexpr
#' @export
#' @return a list contains the UI and  server
#'
#' @examples
#' if(interactive()){
#'     spsInit()
#'     sps_app <- sps(
#'         vstabs = "",
#'         server_expr = {
#'             msg("Hello World", "GREETING", "green")
#'         }
#'     )
#' }
sps <- function(vstabs = "", plugin = "", server_expr=NULL, app_path = getwd()){
    assert_that(is.character(vstabs))
    assert_that(is.character(plugin))
    if(any(duplicated(vstabs)))
        spserror(glue("You input duplicated tab IDs: ",
                      "{vstabs[duplicated(vstabs)]}"))
    sps_env <- new.env(parent = globalenv())
    lapply(file.path("R", list.files("R", "\\.[rR]$")),
           function(x) source(x, local = sps_env)) %>%
        invisible()
    if(any(search() %>% str_detect("^sps_env$"))) detach("sps_env")
    attach(sps_env, name = "sps_env", pos = 2, warn.conflicts = FALSE)
    tab_info <- checkSps(app_path)
    plugin_tabs <- tab_info$tab_id[tab_info$plugin %in% plugin]
    if(!length(plugin_tabs) & plugin[1] != "") spswarn("No plugin matched, skip")
    if((length(vstabs) >= 1 & sum(nchar(vstabs))) > 0 | (length(plugin_tabs))){
        spsinfo("Now check input tabs")
        spsinfo("Find tab info ...")
        vstabs <- c(vstabs, plugin_tabs) %>% unique() %>% {.[!. %in% c("", " ")]}
        tabs <- findTabInfo(vstabs) %>% dplyr::as_tibble()
        if(sum(not_in_vs <- tabs$tpye != 'vs') > 0)
            spserror(glue("Tab '{glue_collapse(vstabs[not_in_vs], ', ')}'",
                          "is/are not visulization tabs"))
        tabs_data <- tabs %>% dplyr::filter(type_sub == "data")
        tabs_plot <- tabs %>% dplyr::filter(type_sub == "plot")
    } else {
        spsinfo("Using default tabs")
        tabs <- dplyr::tibble()
        tabs_data <- dplyr::tibble()
        tabs_plot <- dplyr::tibble()
    }
    ui <- spsUI(tabs_data, tabs_plot)
    spsinfo("UI created")
    server_expr <- rlang::enexpr(server_expr)
    server <- spsServer(tabs, server_expr)
    spsinfo("Server functions created")
    spsinfo("App starts ...", verbose = TRUE)
    list(ui = ui, server = server)
}



#' Create a SystemPipeShiny project
#' @description To run a SPS app, you need to first create a SPS project, a
#' directory contains the required files.
#' @param app_path path, a directory where do you want to create this project,
#' must exist.
#' @param project_name Your project name, default is `SPS_` + `time`
#' @param database_name project database name, recommend to use the default
#'  name: "sps.db". It is used to store app meta information, see [spsDb()]
#' @param overwrite bool, overwrite the `app_path` if there is a folder that
#' has the same name as `project_name`?
#' @param change_wd bool, when creation is done, change working directory into
#' the project?
#' @param verbose bool, do you want additional message?
#' @param open_files bool, If `change_wd == TRUE` and you are also in Rstudio,
#' it will open up *global.R* for you
#' @param colorful bool, should message from this function be colorful?
#' @details Make sure you have write permission to `app_path`
#' @importFrom rstudioapi isAvailable navigateToFile
#' @export
#' @return creates the project folder
#' @examples
#' if(interactive()){
#'     spsInit(change_wd = FALSE)
#' }
spsInit <- function(app_path=getwd(),
                    project_name = glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
                    database_name = "sps.db",
                    overwrite = FALSE,
                    change_wd = TRUE,
                    verbose = FALSE,
                    open_files = TRUE,
                    colorful = TRUE
){
    options(sps=list(verbose = verbose, use_crayon = colorful))
    if(is.writeable(app_path))
        spsinfo("Start to create a new SPS project", TRUE)
    else spserror(glue("{app_path} is not writeable."))

    project_dir <- file.path(app_path, project_name)
    if(dir.exists(project_dir) & !overwrite) {
        spserror(glue("{project_dir} exists"))
    } else if(dir.exists(project_dir) & overwrite) {
        spswarn(glue("Overwrite things in {project_dir}"))
    } else {
        spsinfo(glue("Create project under {project_dir}"), TRUE)
        dir.create(project_dir)
    }

    spsinfo("Now copy files", TRUE)
    copySPSfiles("app/www/", project_dir, TRUE, overwrite, verbose)
    copySPSfiles("app/config", project_dir, TRUE, overwrite, verbose)
    copySPSfiles("app/R", project_dir, TRUE, overwrite, verbose)
    copySPSfiles("app/data", project_dir, TRUE, overwrite, verbose)
    copySPSfiles("app/results", project_dir, TRUE, overwrite, verbose)
    copySPSfiles("app/README.md", project_dir, FALSE, overwrite, verbose)
    copySPSfiles("app/deploy.R", project_dir, FALSE, FALSE, verbose)
    copySPSfiles("app/app_ez/server.R", project_dir, FALSE, overwrite, verbose)
    copySPSfiles("app/app_ez/global.R", project_dir, FALSE, overwrite, verbose)
    copySPSfiles("app/app_ez/ui.R", project_dir, FALSE, overwrite, verbose)
    copySPSfiles("app/app_ez/server.R", project_dir, FALSE, overwrite, verbose)

    spsinfo("Create SPS database", TRUE)
    suppressWarnings(
        spsDb$new()$createDb(db_name=file.path(project_dir,
                                               "config",
                                               database_name))
    )
    if(change_wd) {
        spsinfo(glue("Change working directory to {project_dir}"))
        setwd(project_dir)
        if(rstudioapi::isAvailable() & open_files){
            rstudioapi::navigateToFile("global.R")
        }
    }
    msg("SPS project setup done!", "SPS-INFO", "green")
}


#' Pre start SPS checks
#'
#' @param app_path where is the app directory root location, default current
#' working folder
#' @noRd
# @examples
# checkSps()
checkSps <- function(app_path = getwd()) {
    resolveOptions(app_path)
    checkTabs(app_path)
}

#' @importFrom yaml yaml.load_file
#' @noRd
verifyConfig <- function(app_path) {
    sps_options <-
        yaml::yaml.load_file(glue("{app_path}/config/sps_options.yaml"))
    # can't use vapply, mix types of returns
    sps_defaults <- sapply(names(sps_options),
                           function(x) sps_options[[x]][['default']],
                           simplify = FALSE)
    vapply(seq_along(sps_defaults),
           function(x) if(length(sps_defaults[x]) != 1)
           {
               spswarn(glue("Default for option {names(sps_defaults)[x]} ",
                        "has more than one value: {sps_defaults[x]}"))
               return(FALSE)
           } else (return(TRUE)),
           TRUE
    ) %>%
        {if (!all(.)) spserror("Unexpected config file, see SPS-WARNING")}
    return(list(sps_options, sps_defaults))
}

#' Resolve SPS options
#' need to use the sps_options.yaml file
#' @param app_path default current working folder
#' @noRd
# @examples
# spsInit()
# options(sps = list(mode = c("asas", "sas"),
#                    place = "here", time = c("now", "then"),
#                    loading_screen = FALSE))
# resolveOptions()
resolveOptions <- function(app_path = getwd()){
    ops <- options()$sps
    ops$app_path <- NULL
    verified_ops <- verifyConfig(app_path)
    sps_options <- verified_ops[[1]]
    sps_defaults <- verified_ops[[2]]

    spsinfo(glue("App has {length(sps_defaults)} default configs, ",
             "resolving {length(ops)} custom configs"))
    if (!is.list(ops)) {
        spswarn("Options are not in a list, reset all")
        return(options(sps = sps_defaults))
    }
    # check length
    for (x in seq_along(ops)) {
        if (length(ops[[x]]) != 1) {
            spswarn(glue("option '{names(ops)[x]}' has length not 1 or NULL,",
                     "will be set to default"))
            ops[[x]] = ""
        }
    }
    # check if in option list
    for (x in names(ops)){
        if(is.null(sps_options[[x]]))
            {spswarn(glue("option {x} unknown, skip")); next}
        if("*" %in% sps_options[[x]] & ops[[x]] != sps_defaults[[x]]){
            spsinfo(glue("Option {x} can be any value,",
                         "overwrite default to '{ops[[x]]}'"), TRUE)
            opt_value <- ops[[x]]
        } else{
            opt_value <- if (!ops[[x]] %in% sps_options[[x]] %>% unlist()) {
                spswarn(glue(
                    "option'{x}' has unknown value '{ops[[x]]}', ",
                    "set to default. \n valid values are ",
                    "{glue_collapse(c(sps_options[[x]]), sep=', ')}"
                ))
                sps_defaults[[x]]
            } else {ops[[x]]}
        }
        if (is.null(opt_value)) spswarn(glue("option'{x}' unknown, not used"))
        ops[[x]] <- opt_value
    }
    # replace defaults
    for (x in names(ops)){
        sps_defaults[[x]] <- ops[[x]]
    }
    # add hidden app_path
    sps_defaults[['app_path']] <- app_path
    # replace global env
    options(sps = sps_defaults)
}

#' Print SPS default options
#' @description  Make sure you have created the app directory and it
#' has *config/config.yaml* file
#' @param app_path path, where is the app directory
#'
#' @export
#' @return cat to console the default options
#' @examples
#' if(interactive()){
#'     spsInit(open_files = FALSE)
#'     viewSpsDefaults()
#' }
viewSpsDefaults <- function(app_path = getwd()){
    sps_defaults <- verifyConfig(app_path)[[1]]
    cat(glue("{names(sps_defaults)}: {sps_defaults}\n\n"))
}

#' Check sps tab file on start
#' @importFrom vroom vroom cols col_character
#' @param app_path App dir
#' @noRd
checkTabs <- function(app_path, warn_img = TRUE){
    spsinfo("Now check the tab info in tabs.csv ")
    tab_info <- tryCatch(suppressMessages(
        vroom::vroom(
            file.path(app_path, "config", "tabs.csv"),
            comment = "#",
            altrep = FALSE,
            delim = ",",
            col_types= vroom::cols(image = vroom::col_character()),
            na = character())
        ),
        error = function(e){
            spserror(c(e$message, "\n",
                       "Cannot read ",
                       file.path(app_path, "config", "tabs.csv")))
        })
    cols <- c("tab_id", "display_label","type",
              "type_sub", "image", "tab_file_name",
              "plugin")
    col_check <- cols %in% names(tab_info)
    if(!all(col_check)){
        spserror(glue('Following column(s) missing
                      {glue_collapse(cols[!col_check], ", ")}'))
    }
    ta_dup <- tab_info$tab_id[base::duplicated(tab_info$tab_id)] %>% unique()
    if(length(ta_dup) > 0){
        spserror(glue("Tabname must be unique, find duplicates name(s)",
                      "'{paste(ta_dup, collapse = ', ')}'"))
    }
    no_img <- tab_info$tab_id[tab_info$type_sub == "plot" &
                              tab_info$image == ""]
    if(length(no_img) > 0 & warn_img){
        spswarn(glue("These plot tabs has no image path:
                  '{paste(no_img, collapse = ', ')}'
                  It is recommended to add an image. It will be used",
                  "to generate gallery. Now an empty image is used for ",
                  "these tabs' gallery."))
    }
    spsinfo("tab.csv info check pass")
    return(tab_info)
}

# internal function for `sps` to copy files
copySPSfiles <- function(file_path,
                       project_dir,
                       is_dir=TRUE,
                       overwrite=FALSE,
                       verbose = FALSE){
    sps_path <- system.file(file_path, package = "systemPipeShiny")

    if(sps_path == "")
        spserror(glue("Can't find required SPS files {file_path}.
                       Did you install SPS correctly?"))
    app_path <- file.path(project_dir, basename(sps_path))
    if(dir.exists(app_path) | !is_dir){
        file.copy(sps_path, project_dir,
                  recursive = TRUE, overwrite = overwrite)
    } else {
        dir.create(app_path)
        file.copy(sps_path, project_dir,
                  recursive = TRUE, overwrite = overwrite)
    }
    spsinfo(glue("File(s) copied for {app_path}"), verbose)
}


#' Get or set SPS options
#'
#' @param opt string, length 1, what option you want to get or set
#' @param value if this is not `NULL`, this function will set the
#' option you choose to this value
#' @param empty_is_false bool, when trying to get an option value, if the
#' option is `NULL`, `NA`, `""` or length is 0, return `FALSE`?
#' @return return the option value if value exists; return `FALSE` if the value
#' is empty, like `NULL`, `NA`, `""`; return `NULL` if `empty_is_false = FALSE`;
#'  see [emptyIsFalse]
#'
#'  If `value != NULL` will set the option to this new value, no returns.
#' @export
#' @seealso [viewSpsDefaults()] for options you can view or set
#' @examples
#' spsOption("test1", 1)
#' spsOption("test1")
#' spsOption("test2")
#' spsOption("test2", empty_is_false = FALSE)
spsOption <- function(opt, value = NULL, empty_is_false = TRUE){
    assert_that(is.character(opt) & length(opt) == 1)
    if(not_empty(value))
        options(sps = getOption('sps') %>% {.[[opt]] <- value; .})
    else {
        get_value <- getOption('sps')[[opt]]
        if(!emptyIsFalse(get_value)){
            if(empty_is_false) FALSE
            else get_value
        }
        else get_value
    }
}


#' View SPS project 'config/tabs.csv' information
#'
#' @param return_type one of 'print', 'data', 'colnames', or a specified column
#' name
#' @param n_print how many lines of tab info you want to print out
#' @param app_path SPS project root
#' @details
#' - 'print' will print out the entire *tabs.csv*, you can
#' specify `n_print` for how many lines you want to print;
#' - 'data' will return the tab info tibble
#' - 'colnames' will return all column names of tab info file
#' - A column name will extract the specified column out and return as a vector
#' @return return depends on `return_type`
#' @export
#'
#' @examples
#' spsInit(project_name = "SPS_tabinfo", overwrite = TRUE,
#'         change_wd = FALSE, open_files = FALSE)
#' # all lines
#' spsTabInfo("print", app_path = "SPS_tabinfo")
#' # 5 lines
#' spsTabInfo("print", app_path = "SPS_tabinfo", n_print = 5L)
#' spsTabInfo("data", app_path = "SPS_tabinfo")
#' spsTabInfo("colnames", app_path = "SPS_tabinfo")
#' spsTabInfo("tab_id", app_path = "SPS_tabinfo")
spsTabInfo <- function(return_type = "print", n_print = 40, app_path = getwd()){
    assert_that(is.numeric(n_print))
    assert_that(is.character(return_type))
    tab_info <- checkTabs(app_path, warn_img = FALSE)
    switch(
        return_type,
        "print" = print(tab_info, n = n_print),
        "data" = tab_info,
        "colnames" = colnames(tab_info),
        {
            if(return_type %in% colnames(tab_info)) {tab_info[[return_type]]}
            else spswarn(c("Cannot find column ", return_type))
        }
           )
}
