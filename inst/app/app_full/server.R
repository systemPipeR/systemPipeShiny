####### Server
# DO NOT delete the next line
# last change date: 20200713114337
# please do not delete comments starting with '##'
server <- function(input, output, session) {
    # add a container to communicate tabs
    shared <- reactiveValues()
    # core tabs
    core_dashboardServer("core_dashboard", shared)
    core_topServer("core_top", shared)
    # core_rightServer("core_right", shared)
    core_canvasServer("core_canvas", shared)
    core_aboutServer("core_about", shared)
    # WF tabs server
    wf_mainServer("wf_main", shared)
    wf_targetServer("wf_targets", shared)
    wf_wfServer("wf_wf", shared)
    wf_configServer("wf_config", shared)
    wf_runServer("wf_run", shared)
    # VS tabs
    vs_mainServer("vs_main", shared)
    devComponents("server", shared = shared) # for templates
    ## data
    df_targetsServer("df_targets", shared)
    df_countServer("df_count", shared)
    df_degcountServer("df_degcount", shared)
    df_edgeRServer("df_edgeR", shared)
    ## plots
    plot_pcaServer("plot_pca", shared)
    plot_boxServer("plot_box", shared)
    plot_tsneServer("plot_tsne", shared)
    plot_glmServer("plot_glm", shared)
    plot_mdsServer("plot_mds", shared)
    plot_heatServer("plot_heat", shared)
    plot_clustServer("plot_clust", shared)
    plot_volcanoServer("plot_volcano", shared)

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
    # spsWarnings(session)
    # TODO admin page, come back in next release
    admin_url <- reactive({
        names(getQueryString())
    })
    observe({
        req(admin_url() == spsOption('admin_url'))
        req(spsOption('admin_page'))
        shinyjs::hide("page_user", asis = TRUE)
        shinyjs::show("page_admin", asis = TRUE)
        output$page_admin <- renderUI(adminUI())
    })

    # observeEvent(input$reload, ignoreInit = TRUE, {
    #     sps_options <- getOption('sps')
    #     sps_options[['loading_screen']] = isolate(input$change)
    #     options(sps = sps_options)
    #     server_file <- readLines("server.R", skipNul = FALSE)
    #     server_file[3] <- glue("# last change date: {format(Sys.time(), '%Y%m%d%H%M%S')}")
    #     writeLines(server_file, "server.R")
    #     ui_file <- readLines("ui.R", skipNul = FALSE)
    #     ui_file[3] <- glue("# last change date: {format(Sys.time(), '%Y%m%d%H%M%S')}")
    #     writeLines(ui_file, "ui.R")
    # })
}

