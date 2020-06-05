####### Server
# please do not delete comments starting with '##'
server <- function(input, output, session) {
    shared <- reactiveValues()
    callModule(dashboardServer, "dashboard", shared = shared)
    # WF tabs server
    callModule(wf_mainServer, "wf_main", shared = shared)
    callModule(targetServer, "wf_targets", shared = shared)
    callModule(wfServer, "wf_wf", shared = shared)
    callModule(configServer, "wf_config", shared = shared)
    # VS tabs
    callModule(vs_mainServer, "vs_main", shared = shared)
    devComponents("server", shared) # for templates
    ## data
    callModule(df_targetsServer, "df_targets", shared = shared)
    callModule(df_countServer, "df_count", shared = shared)
    callModule(df_degcountServer, "df_degcount", shared = shared)
    callModule(df_edgeRServer, "df_edgeR", shared = shared)
    ## plots
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

    # global server logic, usually no need to change below
    ## pushbar set up
    setup_pushbar()
    ## loading screening
    serverLoadingScreen(input, output, session)
    ## for workflow control panel
    removeClass(id = "wf-panel", asis = TRUE, class = "tab-pane")
    shared$wf_flags <- data.frame(targets_ready = FALSE, wf_ready = FALSE,
                                  wf_conf_ready = FALSE)
    observeEvent(input$left_sidebar, {
        print(input$left_sidebar)
        toggleClass(id = "wf-panel", class = "shinyjs-hide", asis = TRUE,
                    condition = !str_detect(input$left_sidebar, "^wf_"))
    })
    shared$wf_flags <- data.frame(targets_ready = FALSE, wf_ready = FALSE, wf_conf_ready = FALSE)
    output$wf_panel <- wfProgressPanel(shared)
}
