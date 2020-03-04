####### Server 
server <- function(input, output, session) {
    shared <- reactiveValues() 
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

