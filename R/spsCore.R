
# Initiation, creating tabs etc.

#' @import shiny spsUtil spsComps drawer
#' @importFrom plotly plotlyOutput
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinytoastr toastr_success toastr_info
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom shinyjs show
#' @importFrom DT renderDT datatable
#' @importFrom shiny validate
#' @importFrom shinyjs show hide toggleState
#' @importFrom shinytoastr toastr_success
#' @importFrom methods is
#' @importFrom DT DTOutput
#' @importFrom shinyWidgets radioGroupButtons pickerInput progressBar updateProgressBar
#' @importFrom stringr str_split str_remove_all str_replace_all str_which
#' @importFrom stringr str_remove str_which str_extract str_replace str_sort
#' @importFrom stringr str_detect str_pad
#' @importFrom magrittr %>%
#' @importFrom glue glue glue_collapse
#' @importFrom assertthat assert_that not_empty is.writeable
#' @importFrom ggplot2 ggplot geom_point ggtitle aes geom_bar coord_flip
#' @importFrom stats relevel cutree
#' @importFrom utils capture.output write.csv read.delim packageVersion
#' @importFrom dplyr count
#' @importFrom shinytoastr toastr_warning toastr_error
#' @importFrom htmltools doRenderTags
#' @importFrom shinydashboardPlus box
NULL

utils::globalVariables(c(
    ".", ".module_pkgs"
))


######################### SPS main functions #############################

#' SystemPipeShiny app main function
#' @param tabs custom visualization tab IDs that you want to display, in a character
#' vector. Use [spsTabInfo()] to see what tab IDs you can load
#' @param server_expr additional top level sever expression you want
#' to run. This will run after the default server expressions. It means you can
#' have access to internal server expression objects, like the
#' [shiny::reactiveValues()]
#' object `shared`. You can also overwrite other values. Read "shared object" in
#' manual.
#' @param login_message a shiny tag that will be displayed on the top of login panel,
#' default is a H3 title with text "User login", `shiny::h3("User login")`. If you
#' need more information, you can do something like `div(h3("Login"), p("Some more message))`.
#' @param app_path SPS project path
#' @details You must set the project root as working directory for this
#' function to find required files.
#' @importFrom dplyr as_tibble filter tibble
#' @importFrom rlang enexpr
#' @export
#' @return a list contains the UI and  server
#' @details
#' #### About this function
#' Usually you call this function inside the *global.R* file when SPS initialization
#' is done. This function does not contain too many options. Most choices are
#' controlled by SPS options which are also listed in *global.R* (some lines before
#' calling this function in that file).
#' @examples
#' if(interactive()){
#'     spsInit()
#'     sps_app <- sps(
#'         tabs = "",
#'         server_expr = {
#'             msg("Hello World", "GREETING", "green")
#'         }
#'     )
#' }
sps <- function(
    tabs = "",
    server_expr=NULL,
    login_message = shiny::h3("User login"),
    app_path = getwd()
    ){
    # check login message
    stopifnot(inherits(login_message, "shiny.tag"))
    # check tabs
    assert_that(is.character(tabs))
    if(any(duplicated(tabs)))
        spserror(glue("You input duplicated tab IDs: ",
                      "{tabs[duplicated(tabs)]}"))
    sps_env <- new.env(parent = globalenv())
    r_folder <- file.path(app_path, "R")
    lapply(file.path(r_folder, list.files(r_folder, "\\.[rR]$")),
           function(x) withCallingHandlers(
               source(x, local = sps_env),
               error = function(e) {
                   spswarn(c("Error on sourcing ", x))
                   spswarn(e)
                   if(spsOption('traceback')) printTraceback(sys.calls())
            })
    )  %>% invisible()
    if(any(search() %>% str_detect("^sps_env$"))) detach("sps_env")
    attach(sps_env, name = "sps_env", pos = 2, warn.conflicts = FALSE)
    tab_info <- checkSps(app_path)

    if(length(tabs) >= 1 & sum(nchar(tabs)) > 0){
        spsinfo("Now check input tabs")
        spsinfo("Find tab info ...")
        tabs <- c(tabs) %>% unique() %>% {.[!. %in% c("", " ")]}
        tabs <- findTabInfo(tabs, tab_file = file.path(app_path, "config", "tabs.csv"), force_reload = TRUE) %>% dplyr::as_tibble()
        if(sum(not_in_vs <- tabs$tpye != 'vs') > 0)
            spserror(glue("Tab '{glue_collapse(tabs[not_in_vs], ', ')}'",
                          "is/are not custom tabs"))

    } else {
        spsinfo("Using default tabs")
        tabs <- dplyr::tibble()
    }
    # check for required module pkgs
    missings <- checkModulePkgs()
    if(missings %>% unlist %>% length() > 0) spswarn("You have missing packages, some modules will not be loaded")

    # check guide
    spsinfo("check guide")
    guide <- parseGuide()

    # load UI
    ui <- spsUI(tabs, missings, sps_env, guide, login_message)
    spsinfo("UI created")
    mainUI <- if (!is.null(ui[['login']])) ui[['main']] else NULL
    # load server
    server_expr <- rlang::enexpr(server_expr)
    server <- spsServer(tabs, server_expr, missings, sps_env, guide, mainUI)
    spsinfo("Server functions created")
    spsinfo("App starts ...", verbose = TRUE)
    # return in a list to be called
    list(ui = if (is.null(ui[['login']])) ui[['main']] else ui[['login']], server = server)
}



#' Create a SystemPipeShiny project
#' @description To run a SPS app, you need to first create a SPS project, a
#' directory contains the required files.
#' @param app_path path, a directory where do you want to create this project,
#' must exist.
#' @param project_name Your project name, default is `SPS_` + `time`
#' @param database_name deprecated in current version.
#' project database name, recommend to use the default
#'  name: "sps.db". It is used to store app meta information.
#' @param overwrite bool, overwrite the `app_path` if there is a folder that
#' has the same name as `project_name`?
#' @param change_wd bool, when creation is done, change working directory into
#' the project?
#' @param verbose bool, do you want additional message?
#' @param open_files bool, If `change_wd == TRUE` and you are also in Rstudio,
#' it will open up *global.R* for you
#' @param colorful bool, should message from this function be colorful?
#' @details Make sure you have write permission to `app_path`.
#'
#' The database in not used in current version.
#'
#' @importFrom rstudioapi isAvailable navigateToFile
#' @export
#' @return creates the project folder
#' @examples
#' if(interactive()){
#'     spsInit(change_wd = FALSE)
#' }
spsInit <- function(app_path=getwd(),
                    project_name = glue::glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
                    database_name = "sps.db",
                    overwrite = FALSE,
                    change_wd = TRUE,
                    verbose = FALSE,
                    open_files = TRUE,
                    colorful = TRUE
){
    stopifnot(dir.exists(app_path))
    stopifnot(is.character(project_name) && length(project_name) == 1)
    stopifnot(is.character(database_name) && length(database_name) == 1)
    stopifnot(is.logical(overwrite) && length(overwrite) == 1)
    stopifnot(is.logical(change_wd) && length(change_wd) == 1)
    stopifnot(is.logical(verbose) && length(verbose) == 1)
    stopifnot(is.logical(open_files) && length(open_files) == 1)
    stopifnot(is.logical(colorful) && length(colorful) == 1)

    opts_old <- list(
        verbose = spsOption("verbose"),
        colorful = spsOption("use_crayon")
    )
    spsOption("verbose", verbose); spsOption("use_crayon", colorful)
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
    spsOption("verbose", opts_old$verbose); spsOption("use_crayon", opts_old$colorful)
}


################### Internal setups ###########################

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
    sps_options <- yaml::yaml.load_file(glue("{app_path}/config/sps_options.yaml"))
    # can't use vapply, mix types of returns
    sps_defaults <- lapply(names(sps_options), function(x) sps_options[[x]][['default']])
    names(sps_defaults) <- names(sps_options)
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
#                    login_screen = FALSE))
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

#' Print SPS options
#' @description  Make sure you have created the app directory and it
#' has *config/config.yaml* file.
#'
#' [spsOptDefaults] prints out all default and other avaliable values for
#' each option. [spsOptions] print all current set option values.
#'
#' Note: the [spsUtil::spsOption] is used to get or set a **single** option value.
#' [spsOptions] is used to print **all** current option values. If you need to
#' set all values at once, use the *global.R* file under SPS project root.
#' @param app_path path, where is the app directory
#'
#' @export
#' @return cat to console SPS option values
#' @examples
#' if(interactive()){
#'     # start a SPS project
#'     spsInit(open_files = FALSE)
#'     viewSpsDefaults()
#'     # change a few options
#'     options(sps = list(
#'         mode = "server",
#'         warning_toast = TRUE,
#'         login_screen = FALSE,
#'         login_theme = "vhelix",
#'         use_crayon = TRUE
#'     ))
#'     # view current options
#'     spsOptions()
#' }
spsOptDefaults <- function(app_path = getwd()){
    sps_defaults <- verifyConfig(app_path)[[1]]
    option_names <- names(sps_defaults)
    for (i in seq_along(sps_defaults)) {
        option_names[i] %>% crayon::blue$bold() %>% {cat(., ":\n", sep = "")}
        cat(crayon::green$bold("    Default: ")); cat(sps_defaults[[i]][['default']], "\n")
        cat(crayon::green$bold("    Other: ")); cat(sps_defaults[[i]][['other']], "\n")
    }
    cat("* means any value will be accepted\n")
}

#' @rdname spsOptDefaults
#' @param show_legend bool, show the color legend?
#' @importFrom crayon green blue make_style chr
#'
#' @return
#' @export
#'
spsOptions <- function(app_path = getwd(), show_legend = TRUE){
    if(!file.exists(glue("{app_path}/config/sps_options.yaml"))) {
        spserror(glue("{app_path}/config/sps_options.yaml does not exist"))
    }
    sps_defaults <- verifyConfig(app_path)[[2]]
    default_names <- names(sps_defaults)
    sps_values <- getOption("sps")
    option_names <- names(sps_values)
    cat(crayon::green$bold("Current project option settings:"), "\n")
    for (i in seq_along(sps_values)) {
        if(option_names[i] %in% default_names) {
            title_color <- crayon::blue$bold
            title_suffix <- ""
            if(identical(sps_values[[i]], sps_defaults[[option_names[i]]])){
                value_color <- crayon::green$bold
                value_suffix <- ""
            }  else {
                value_color <- crayon::chr
                value_suffix <- "+"
            }
        } else {
            title_suffix <- "*"
            title_color <-  crayon::make_style("orange")$bold
            value_color <-  crayon::make_style("orange")
            value_suffix <- "+"
        }
        paste0(option_names[i], title_suffix) %>% title_color() %>% {cat(., ":\n", sep = "")}
        cat("    ", value_color(paste0(sps_values[[i]]), value_suffix), "\n", sep = "")
    }
    if(show_legend){
        cat(
            "********\nOption legends:\n",
            crayon::blue$bold("    known options    "),
            crayon::make_style("orange")$bold("    Hidden/custom options* and values+\n"),
            "Value legends:\n",
            crayon::green$bold("    same as default values    "),
            "    different from defaults+",
            sep = ""
        )
    }
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
            col_types= vroom::cols(image = vroom::col_character(),
                                   displayed = vroom::col_character()),
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
        spswarn(glue("These custom tabs has no image path:
                  '{paste(no_img, collapse = ', ')}'
                  It is recommended to add an image. It will be used ",
                  "to generate gallery on the custom visualization main tab. Now an empty image is used for ",
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

# declaim pkg check var
.module_pkgs <- list(
    wf = sort(c(
        'systemPipeR',
        'systemPipeRdata',
        # 'networkD3',
        'rhandsontable',
        'zip',
        'callr',
        'pushbar',
        'fs',
        'readr',
        'R.utils',
        # 'DOT',
        # 'shinyTree',
        "openssl"
    )),
    rna = sort(c(
        'DESeq2',
        'systemPipeR',
        'SummarizedExperiment',
        'glmpca',
        'pheatmap',
        'grid',
        'ape',
        'Rtsne',
        'UpSetR',
        'tidyr'
    )),
    ggplot = sort(c(
        'esquisse'
    ))
)


checkModulePkgs <- function(){
    list(
        wf = checkModulePkgs_internal("module_wf", .module_pkgs[['wf']], "workflow"),
        rna = checkModulePkgs_internal("module_rnaseq", .module_pkgs[['rna']], "RNA-Seq"),
        ggplot = checkModulePkgs_internal("module_ggplot", .module_pkgs[['ggplot']], "Quick ggplot")
    )
}

checkModulePkgs_internal <- function(module_name, pkgs, mol_title){
    if(spsOption(module_name)){
        missings <- spsUtil::checkNameSpace(pkgs, quietly = TRUE)
        missing_str <- glue_collapse(missings, '", "')
        if(emptyIsFalse(missings)){
            spswarn(c('You are loading the ', mol_title, ' module but missing packages: "',
                      missing_str, '", run:'))
            cat(glue(
                '
                if (!requireNamespace("BiocManager", quietly=TRUE))
                    install.packages("BiocManager")
                BiocManager::install(c("{missing_str}"))\n
                '
            ))
        }
        glue(
        '
        ```r
        if (!requireNamespace("BiocManager", quietly=TRUE))
            install.packages("BiocManager")
        BiocManager::install(c("{missing_str}"))
        ```
        '
        )
    } else NULL
}









