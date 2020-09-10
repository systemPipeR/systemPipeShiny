######################### SPS main function #############################
# Initiation, creating tabs etc.


#' @import shiny assertthat stringr magrittr glue ggplot2 shinyTree
NULL


#' SystemPipeShiny app start function
#' @param vstabs your custom visualization tabs you want to display
#'
#' @param server_expr additional top level sever expression you want
#' to run. This will run after the default server expressions. It means you can
#' have access to internal server expression objects, like the reactiveValues
#' object `shared`. You can also overwrite other values.
#' @importFrom dplyr as_tibble filter tibble
#' @importFrom rlang enexpr
#' @export
#' @return a list contains the UI and  server
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
sps <- function(vstabs, server_expr=NULL){
    assert_that(is.character(vstabs))
    if(any(duplicated(vstabs)))
        spserror(glue("You input duplicated tab IDs: ",
                      "{vstabs[duplicated(vstabs)]}"))
    sps_env <- new.env(parent = globalenv())
    lapply(file.path("R", list.files("R", "\\.[rR]$")),
           function(x) source(x, local = sps_env)) %>%
        invisible()
    if(any(search() %>% str_detect("^sps_env$"))) detach("sps_env")
    attach(sps_env, name = "sps_env", pos = 2, warn.conflicts = FALSE)
    checkSps()
    if(length(vstabs) >= 1 & sum(nchar(vstabs)) > 0){
        spsinfo("Now check input tabs")
        spsinfo("Find tab info ...")
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
#'
#' @param dir_path path, where do you want to create this project
#' @param project_name Your project name
#' @param database_name project database name, recommend to use the default name
#' @param overwrite bool, overwrite the `dir_path`?
#' @param change_wd bool, when creation is done, change working directory?
#' @param verbose bool, do you want additional message?
#' @param open_files bool, If `change_wd == TRUE` and you are also in Rstudio,
#' it will open up global.R, ui.R and server.R for you
#' @param colorful bool, should message from this function be colorful?
#' @importFrom rstudioapi isAvailable navigateToFile
#' @export
#' @return creates the project folder
#' @examples
#' if(interactive()){
#'     spsInit(change_wd = FALSE)
#' }
spsInit <- function(dir_path=getwd(),
                    project_name = glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
                    database_name = "sps.db",
                    overwrite = FALSE,
                    change_wd = TRUE,
                    verbose = FALSE,
                    open_files = TRUE,
                    colorful = TRUE
){
    options(sps=list(verbose = verbose, use_crayon = colorful))
    if(is.writeable(dir_path))
        spsinfo("Start to create a new SPS project", TRUE)
    else spserror(glue("{dir_path} is not writeable."))

    project_dir <- file.path(dir_path, project_name)
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
            # rstudioapi::navigateToFile("server.R")
            # rstudioapi::navigateToFile("ui.R")
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
    return(TRUE)
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

#' Print systemPipeShiny default Options
#' @description  Make sure you created the app folder and has config.yaml
#' in config folder
#' @param app_path where is the app directory
#'
#' @export
#' @return cat to console the default options
#' @examples
#' if(interactive()){
#'     spsInit()
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
checkTabs <- function(app_path){
    spsinfo("Now check the tab info in tabs.csv ")
    tab_info <-suppressMessages(
        vroom::vroom(
            file.path(app_path, "config", "tabs.csv"),
            comment = "#",
            altrep = FALSE,
            delim = ",",
            col_types= vroom::cols(image = vroom::col_character()),
            na = character())
    )
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
    if(length(no_img) > 0){
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
    dir_path <- file.path(project_dir, basename(sps_path))
    if(dir.exists(dir_path) | !is_dir){
        file.copy(sps_path, project_dir,
                  recursive = TRUE, overwrite = overwrite)
    } else {
        dir.create(dir_path)
        file.copy(sps_path, project_dir,
                  recursive = TRUE, overwrite = overwrite)
    }
    spsinfo(glue("File(s) copied for {dir_path}"), verbose)
}


#' Get or set SPS options
#'
#' @param opt string, length 1, what option you want to get or set
#' @param value if this is not `NULL`, this function will set the
#' option you choose to this value
#' @param empty_is_false bool, when you get an option, if the option is
#' `NULL`, `NA`, `""` or length is 0, return `FALSE`?
#' @return option value if value exists; `FALSE` if the value is empty,
#' like `NULL`, `NA`, `""`; `NULL` if `empty_is_false = FALSE`;
#'  see [emptyIsFalse]
#'
#'  If `value != NULL` will set the value, no returns.
#' @export
#'
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
