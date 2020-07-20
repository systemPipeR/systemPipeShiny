# please do not delete comments starting with '##'
# header
dashboardHeader <- dashboardHeaderPlus(
    title = tagList(
        span(class = "logo-lg", "systemPipeShiny"),
        img(src = "img/systemPipe_small.png")
    ),
    enable_rightsidebar = FALSE,
    rightSidebarIcon = "clipboard-check",
    left_menu = core_topUI("core_top")
)
# side bar
dashboardSidebar <-  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(id = "left_sidebar",
        menuItem("Dashboard", tabName = "core_dashboard", icon = icon("sitemap")
                ),
        menuItem(id = 'wf-control',
            HTML('Workflow Mangement<small class="badge pull-right bg-olive">Beta</small>'), tabName = "wf_main",
            menuSubItem(text = "Targets", tabName = "wf_targets", ),
            menuSubItem(text = "Workflow File", tabName = "wf_wf"),
            menuSubItem(text = "Workflow Config", tabName = "wf_config"),
            menuSubItem(text = "Run Workflow", tabName = "wf_run")
                 ),
        menuItem(
            "Visualization", icon = icon("images"), tabName = "vs_main",
            menuItem(
                text = "Prepare dataset",
                ## vs dfs add to sidebar
                devComponents("ui_menu_df"),
                menuSubItem(text = "Targets", tabName = "df_targets"),
                menuSubItem(text = "Count data", tabName = "df_count"),
                menuSubItem(text = "DEG Count data", tabName = "df_degcount"),
                menuSubItem(text = "EdgeR data", tabName = "df_edgeR")
                ),
            menuItem(
                text = "Collection of plots",
                ## vs plots add to sidebar
                devComponents("ui_menu_plot"),
                menuSubItem(text = "PCA Plot", tabName = "plot_pca"),
                menuSubItem(text = "Box Plot", tabName = "plot_box"),
                menuSubItem(text = "t-SNE", tabName = "plot_tsne"),
                menuSubItem(text = "MDS", tabName = "plot_mds"),
                menuSubItem(text = "GLM-PCA", tabName = "plot_glm"),
                menuSubItem(text = "heat map", tabName = "plot_heat"),
                menuSubItem(text = "H-Clust", tabName = "plot_clust"),
                menuSubItem(text = "Volcano", tabName = "plot_volcano")
                )
        ),
        menuItem("Canvas", tabName = "core_canvas", icon = icon("paint-brush")
        ),
        menuItem("About", icon = icon("info"), tabName = "core_about")
    )
)
# body
dashboardBody <- dashboardBody(
    tags$head(
        tags$link(rel="shortcut icon", href="img/systemPipe_small.png"),
        useShinyjs(),
        useSweetAlert(),
        useSps(),
    ),
    tabItems(
        # WF tabs
        tabItem(tabName = "wf_main", wf_mainUI("wf_main")),
        wfPanel(),
        tabItem(tabName = "wf_targets", wf_targetUI("wf_targets")),
        tabItem(tabName = "wf_wf", wf_wfUI("wf_wf")),
        tabItem(tabName = "wf_config", wf_configUI("wf_config")),
        tabItem(tabName = "wf_run", wf_runUI("wf_run")),
        # VS tabs
        tabItem(tabName = "vs_main", vs_mainUI("vs_main")),
        ## vs dfs
        devComponents("ui_tab_df"),
        tabItem(tabName = "df_targets", df_targetsUI("df_targets")),
        tabItem(tabName = "df_count", df_countUI("df_count")),
        tabItem(tabName = "df_degcount", df_degcountUI("df_degcount")),
        tabItem(tabName = "df_edgeR", df_edgeRUI("df_edgeR")),
        ## vs plots
        devComponents("ui_tab_plot"),
        tabItem(tabName = "plot_pca", plot_pcaUI("plot_pca")),
        tabItem(tabName = "plot_box", plot_boxUI("plot_box")),
        tabItem(tabName = "plot_tsne", plot_tsneUI("plot_tsne")),
        tabItem(tabName = "plot_mds", plot_mdsUI("plot_mds")),
        tabItem(tabName = "plot_glm", plot_glmUI("plot_glm")),
        tabItem(tabName = "plot_heat", plot_heatUI("plot_heat")),
        tabItem(tabName = "plot_clust", plot_clustUI("plot_clust")),
        tabItem(tabName = "plot_volcano", plot_volcanoUI("plot_volcano")),
        # core tabs
        tabItem(tabName = "core_dashboard", core_dashboardUI("core_dashboard")),
        tabItem(tabName = "core_canvas", core_canvasUI("core_canvas")),
        tabItem(tabName = "core_about", core_aboutUI("core_about"))
    )
)
# right side bar, not in use at this moment
# rightsidebar <- rightSidebar(
#     background = "light", icon = "clipboard-check", width = 400,
#     core_rightUI("core_right")
# )
# app main UI
mainUI <- dashboardPagePlus(header = dashboardHeader, sidebar = dashboardSidebar,
                            title = "systemPipeShiny",
                            body =  dashboardBody #,rightsidebar = rightsidebar
                            )
# merge everything together
ui <- spsUIwrapper(mainUI)
