####### Server 
server <- function(input, output, session) {
    shared <- reactiveValues() 
    callModule(dashboardServer, "dashboard")
    # WF tabs server
    callModule(wf_mainServer, "wf_main", shared = shared)
    callModule(targetServer, "wf_targets", shared = shared)
    callModule(wfServer, "wf_wf", shared = shared)
    callModule(configServer, "wf_config", shared = shared)
    # VS tabs
    callModule(vs_mainServer, "vs_main", shared = shared)
    callModule(df_rawServer, "df_raw", shared = shared)
    callModule(vs_mainServer, "vs_main", shared = shared)
    callModule(plot_pointServer, "plot_point", shared = shared)
    # other tabs
    callModule(topServer, "top", shared = shared)
    callModule(rightServer, "right", shared = shared)
    callModule(aboutServer, "about")
    # global server logic
    onclick("sidebarItemExpanded", {
        # if (input$left_sidebar == "Workflow") addClass(selector = "body", class = "sidebar-collapse")
    })
    onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
    onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))

}

