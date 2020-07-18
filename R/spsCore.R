######################### SPS main function #############################
# Initiation, creating tabs etc.
#' @import glue
NULL

sps <- function(vstabs, server_expr=NULL){
    assert_that(is.character(vstabs))
    if(length(vstabs) > 1 & sum(nchar(vstabs)) > 0){
        spsinfo("Now check input tabs")
        spsinfo("Find tab info ...")
        tabs <- findTabInfo(vstabs) %>% as_tibble()
        if(sum(not_in_vs <- tabs$tpye != 'vs') > 0)
            spserror(glue("Tab '{glue_collapse(vstabs[not_in_vs], ', ')}' is/are not visulization tabs"))
        tabs_df <- tabs %>% filter(type_sub == "data")
        tabs_plot <- tabs %>% filter(type_sub == "plot")
    } else {
        spsinfo("Using default tabs")
        tabs <- tibble()
        tabs_df <- tibble()
        tabs_plot <- tibble()
    }
    ui <- spsUI(tabs_df, tabs_plot)
    spsinfo("UI created")
    server_expr <- enexpr(server_expr)
    server <- spsServer(tabs, server_expr)
    spsinfo("Server created")
    spsinfo("App starts ...", verbose = TRUE)
    list(ui = ui, server = server)
}






#' Pre start SPS checks
#'
#' @param appDir where is the app directory root location, default current
#' working folder
#' @export
#'
#' @examples
#' checkSps()
checkSps <- function(appDir = getwd()) {
    resolveOptions(appDir)
    checkTabs(appDir)
}

verifyConfig <- function(appDir) {
    sps_options <- yaml::yaml.load_file(glue("{appDir}/config/sps_options.yaml"))
    sps_defaults <- sapply(names(sps_options),
                           function(x) sps_options[[x]][['default']],
                           simplify = F)
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
#' @param appDir default current working folder
#' @example
#' options(sps = list(mode = c("asas", "sas"), place = "here", time = c("now", "then"), loading_screen = FALSE))
#' resolveOptions()
resolveOptions <- function(appDir = getwd()){
    ops <- options()$sps
    ops$appDir <- NULL
    verified_ops <- verifyConfig(appDir)
    sps_options <- verified_ops[[1]]
    sps_defaults <- verified_ops[[2]]

    msg(glue("App has {length(sps_defaults)} default configs, ",
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
            spsinfo(glue("Option {x} can be any value, overwrite default to '{ops[[x]]}'"), TRUE)
            opt_value <- ops[[x]]
        } else{
            opt_value <- if (!ops[[x]] %in% sps_options[[x]] %>% unlist) {
                spswarn(glue(
                    "option'{x}' has unknown value '{ops[[x]]}', set to default. \n",
                    "valid values are {glue_collapse(c(sps_options[[x]]), sep=', ')}"
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
    # check options of suggested packages
    # if (sps_defaults[["display_code"]]) {
    #     if (!requireNamespace("shinymeta", quietly = TRUE)) {
            # msg(c("Option use_shinymeta is `TRUE` but",
            #       "package shinymeta is not installed, turn off this option"),
            #     level = "SPS-WARNING", .other_color = "orange")
    #         sps_defaults[["display_code"]] <- FALSE
    #     }
    # }
    # add hidden appdir
    sps_defaults[['appDir']] <- appDir
    # replace global env
    options(sps = sps_defaults)
}

#' Print systemPipeShiny default Options
#' Make sure you created the app folder and has config.yaml in config folder
#' @param appDir where is the app directory
#'
#' @export
#'
#' @examples
#' viewSpsDefaults()
viewSpsDefaults <- function(appDir = getwd()){
    sps_defaults <- verifyConfig(appDir)[[1]]
    cat(glue("{names(sps_defaults)}: {sps_defaults}\n\n"))
}

#' Check sps tab file on start
checkTabs <- function(appDir){
    spsinfo("Now check the tab info in tab.csv ")
    tab_info <- if (exists("tab_info")) {
        tab_info
    } else {
        suppressMessages(vroom::vroom(glue("{appDir}/config/tabs.csv"),
                                      comment = "#", na = character()))
    }
    ta_dup <- tab_info$tab_name[base::duplicated(tab_info$tab_name)] %>% unique()
    if(length(ta_dup) > 0){
        spserror(glue("Tabname must be unique, find duplicates name(s) '{paste(ta_dup, collapse = ', ')}'"))
    }
    no_img <- tab_info$tab_name[tab_info$type_sub == "plot" & tab_info$image == ""]
    if(length(no_img) > 0){
        spswarn(glue("These plot tabs has no image path:
                  '{paste(no_img, collapse = ', ')}'
                  It is recommended to add an image. It will be used to generate gallery.
                  Now an empty image is used for these tabs' gallery."))
    }
    spsinfo("tab.csv info check pass")
}

