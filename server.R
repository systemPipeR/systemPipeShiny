####### Server 
server <- function(input, output, session) {
    shared <- reactiveValues() 
    # sub tab logic
    callModule(dashboardServer, "dashboard")
    callModule(wf_mainServer, "wf_main", shared = shared)
    callModule(uploadServer, "upload", shared = shared)
    callModule(edaServer, "eda", shared = shared)
    callModule(degServer, "deg", shared = shared)
    callModule(aboutServer, "about")
    callModule(topServer, "top", shared = shared)
    callModule(rightServer, "right", shared = shared)
    callModule(vs_listServer, "vs", shared = shared)
    callModule(vs_mainServer, "vs_main", shared = shared)
    # main server logic
    onclick("sidebarItemExpanded", {
        if (input$left_sidebar == "Workflow") addClass(selector = "body", class = "sidebar-collapse")
    })

}

