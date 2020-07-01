####### Server
# please do not delete comments starting with '##'
server <- function(input, output, session) {
    shared <- reactiveValues()
    # core tabs
    callModule(core_dashboardServer, "core_dashboard", shared = shared)
    callModule(core_topServer, "core_top", shared = shared)
    # callModule(core_rightServer, "core_right", shared = shared)
    core_canvasServer("core_canvas", shared)
    callModule(core_aboutServer, "core_about")
    # WF tabs server
    callModule(wf_mainServer, "wf_main", shared = shared)
    callModule(wf_targetServer, "wf_targets", shared = shared)
    callModule(wf_wfServer, "wf_wf", shared = shared)
    callModule(wf_configServer, "wf_config", shared = shared)
    callModule(wf_runServer, "wf_run", shared = shared)
    # VS tabs
    callModule(vs_mainServer, "vs_main", shared = shared)
    devComponents("server", shared = shared) # for templates
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

    # global server logic, usually no need to change below
    ## pushbar set up
    setup_pushbar()
    ## loading screening
    serverLoadingScreen(input, output, session)
    ## for workflow control panel
    removeClass(id = "wf-panel", asis = TRUE, class = "tab-pane")
    observeEvent(input$left_sidebar, {
        toggleClass(id = "wf-panel", class = "shinyjs-hide", asis = TRUE,
                    condition = !str_detect(input$left_sidebar, "^wf_"))
    })
    shared$wf_flags <- data.frame(targets_ready = FALSE, wf_ready = FALSE, wf_conf_ready = FALSE)
    output$wf_panel <- wfProgressPanel(shared)
}
