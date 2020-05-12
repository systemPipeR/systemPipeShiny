######################### SPS main function #############################

#' @import glue
NULL

runSPS <- function(appDir = ".") {
    source(glue("{appDir}/global.R"))
    resolveOptions(appDir)
    return(runApp(appDir = appDir))
}

#' Resolve SPS options
#' need to use the sps_options.yaml file
#' @example
#' options(sps = list(mode = c("asas", "sas"), place = "here", time = c("now", "then"), loading_screen = FALSE))
#' resolveOptions()
resolveOptions <- function(appDir = "."){
    ops <- options()$sps
    sps_options <- yaml::yaml.load_file(glue("{appDir}/config/sps_options.yaml"))
    sps_defaults <- sapply(names(sps_options), function(x) sps_options[[x]][['default']], simplify = F)
    if (!is.list(ops)) {message("Options are not in a list, reset"); options(sps = sps_defaults)} # return
    # check length
    for (x in seq_along(ops)) {
        if (length(ops[[x]]) != 1) {
            message(glue("option '{names(ops)[x]}' has length not 1 or NULL, will be set to default"))
            ops[[x]] = ""
        }
    }
    # check if in option list
    for (x in names(ops)){
        opt_value <- if (!ops[[x]] %in% sps_options[[x]] %>% unlist) sps_defaults[[x]] else ops[[x]]
        if (is.null(opt_value)) message(glue("option'{x}' unknown, not used"))
        ops[[x]] <- opt_value
    }
    # replace defaults
    for (x in names(ops)){
        sps_defaults[[x]] <- ops[[x]]
    }
    # replace global env
    options(sps = sps_defaults)
}
