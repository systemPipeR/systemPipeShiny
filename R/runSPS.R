######################### SPS main function #############################

#' @import glue
NULL

#' Start systemPipeShiny
#'
#' @param appDir where is the app directory root location, default current
#' working folder
#' @export
#'
#' @examples
#' runSPS()
runSPS <- function(appDir = getwd()) {
    source(glue("{appDir}/global.R"))
    resolveOptions(appDir)
    return(runApp(appDir = appDir))
}

#' Resolve SPS options
#' need to use the sps_options.yaml file
#' @param appDir default current working folder
#' @example
#' options(sps = list(mode = c("asas", "sas"), place = "here", time = c("now", "then"), loading_screen = FALSE))
#' resolveOptions()
resolveOptions <- function(appDir = getwd()){
    ops <- options()$sps
    sps_options <- yaml::yaml.load_file(glue("{appDir}/config/sps_options.yaml"))
    sps_defaults <- sapply(names(sps_options), function(x) sps_options[[x]][['default']], simplify = F)
    msg(glue("{Sys.time()} App has {length(sps_defaults)} default configs, resolving {length(ops)} custom configs"))
    if (!is.list(ops)) {message("Options are not in a list, reset all"); return(options(sps = sps_defaults))}
    # check length
    for (x in seq_along(ops)) {
        if (length(ops[[x]]) != 1) {
            message(glue("option '{names(ops)[x]}' has length not 1 or NULL, will be set to default"))
            ops[[x]] = ""
        }
    }
    # check if in option list
    for (x in names(ops)){
        opt_value <- if (!ops[[x]] %in% sps_options[[x]] %>% unlist) {
            message(glue("
                 option'{x}' has unknown value '{ops[[x]]}', set to default.
                 valid values are {glue_collapse(c(sps_options[[x]]), sep=', ')}
                         "))
            sps_defaults[[x]]
            } else {ops[[x]]}
        if (is.null(opt_value)) message(glue("option'{x}' unknown, not used"))
        ops[[x]] <- opt_value
    }
    # replace defaults
    for (x in names(ops)){
        sps_defaults[[x]] <- ops[[x]]
    }
    # add hidden appdir
    sps_defaults[['appDir']] <- appDir
    # replace global env
    options(sps = sps_defaults)
}
