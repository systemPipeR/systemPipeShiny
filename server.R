####### Server
# please do not delete comments starting with '##'
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
    ## data
    callModule(df_targetsServer, "df_targets", shared = shared)
    callModule(df_rawServer, "df_raw", shared = shared)
    callModule(df_countServer, "df_count", shared = shared)
    callModule(df_degcountServer, "df_degcount", shared = shared)
    callModule(df_edgeRServer, "df_edgeR", shared = shared)
    ## plots
    callModule(plot_pointServer, "plot_point", shared = shared)
    callModule(plot_pcaServer, "plot_pca", shared = shared)
    callModule(plot_boxServer, "plot_box", shared = shared)
    callModule(plot_tsneServer, "plot_tsne", shared = shared)
    callModule(plot_glmServer, "plot_glm", shared = shared)
    callModule(plot_mdsServer, "plot_mds", shared = shared)
    callModule(plot_heatServer, "plot_heat", shared = shared)
    callModule(plot_clustServer, "plot_clust", shared = shared)
    callModule(plot_volcanoServer, "plot_volcano", shared = shared)
    # other tabs
    callModule(topServer, "top", shared = shared)
    callModule(rightServer, "right", shared = shared)
    callModule(aboutServer, "about")
    # global server logic
    setup_pushbar()
    onclick("sidebarItemExpanded", {
        # if (input$left_sidebar == "Workflow") addClass(selector = "body", class = "sidebar-collapse")
    })
    # onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
    # onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
    serverLoadingScreen(input, output, session)
}
