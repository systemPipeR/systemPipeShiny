.onLoad <- function(libname, pkgname) {
    #default options
    sps_options <- try(yaml::yaml.load_file(system.file(package = "systemPipeShiny", "app", "config", "sps_options.yaml")), silent = TRUE)
    if (inherits(sps_options, "try-error")) {
        spswarn("Cannot set some SPS default options on load. Your package installation may have problems.")
        return(invisible())
    }
    sps_defaults <- lapply(names(sps_options), function(x) sps_options[[x]][['default']])
    names(sps_defaults) <- names(sps_options)
    options(sps = sps_defaults)
    invisible()
}
