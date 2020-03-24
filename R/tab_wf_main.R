## UI
wf_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabBox(title = toupper("workflow management"), id = ns("targets"),
               width = 12,
               targetUI(ns("targets")),
               wfUI(ns("wf")),
               configUI(ns("config"))
        )
    )
}

## server
wf_mainServer <- function(input, output, session, shared){
    callModule(targetMod, "targets", shared = shared)
    callModule(wfServer, "wf", shared = shared)
    callModule(configServer, "config", shared = shared)
}
