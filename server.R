####### Server 
server <- function(input, output, session) {
    shared <- reactiveValues(space0 = NULL) # declare a space share objects
    shared$wf_flags <- data.frame(targets_ready = FALSE, wf_ready = FALSE, wf_conf_ready = FALSE) # needed for rightside bar
    callModule(dashboardServer, "dashboard")
    callModule(targetServer, "targets", shared = shared)
    callModule(wfServer, "wf")
    callModule(uploadServer, "upload", shared = shared)
    callModule(edaServer, "eda", shared = shared)
    callModule(degServer, "deg", shared = shared)
    callModule(aboutServer, "about")
    callModule(topServer, "top", shared = shared)
    callModule(rightServer, "right", shared = shared)
}

