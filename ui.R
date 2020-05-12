####### UI
# valid colors:
# red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

# please do not delete comments starting with '##'
# header
dashboardHeader <- dashboardHeaderPlus(
    title = tagList(
        span(class = "logo-lg", "systemPipeShiny"),
        img(src = "systemPipe_small.png"),
        tags$div(
            useShinyjs(),
            useSweetAlert(),
            useToastr(),
            useSps()
        )
    ),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "clipboard-check",
    left_menu = topUI("top")
)
# side bar
dashboardSidebar <-  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(id = "left_sidebar",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("sitemap")),
                badgeLabel = "Main", badgeColor = "red",

        menuItem(
            "Workflow Mangement", icon = icon("tasks"), tabName = "wf_main",
            tags$script("sidebarSpanJump('Workflow Mangement', 'wf_main');"),
            menuSubItem(text = "Targets", tabName = "wf_targets"),
            menuSubItem(text = "Workflow File", tabName = "wf_wf"),
            menuSubItem(text = "Workflow Config", tabName = "wf_config"),
            menuSubItem(text = "Run Workflow", tabName = "wf_run")
                 ),
        menuItem(
            "Visualization", icon = icon("tasks"), tabName = "vs_main",
            tags$script("sidebarSpanJump('Visualization', 'vs_main');"),
            menuItem(
                text = "Prepare dataset",
                ## vs dfs add to sidebar
                menuSubItem(text = "Targets", tabName = "df_targets"),
                menuSubItem(text = "Raw data", tabName = "df_raw"),
                menuSubItem(text = "Count data", tabName = "df_count"),
                menuSubItem(text = "DEG Count data", tabName = "df_degcount"),
                menuSubItem(text = "EdgeR data", tabName = "df_edgeR")
                ),
            menuItem(
                text = "Collection of plots",
                ## vs plots add to sidebar
                menuSubItem(text = "Scatter Plot", tabName = "plot_point"),
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
        ## add other tabs
        menuItem("About", icon = icon("info"), tabName = "about")
    )
)
# body
dashboardBody <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard", dashboardUI("dashboard")),
        # WF tabs
        tabItem(tabName = "wf_main", wf_mainUI("wf_main")),
        tabItem(tabName = "wf_targets", targetUI("wf_targets")),
        tabItem(tabName = "wf_wf", wfUI("wf_wf")),
        tabItem(tabName = "wf_config", configUI("wf_config")),
        # VS tabs
        tabItem(tabName = "vs_main", vs_mainUI("vs_main")),
        ## vs dfs
        tabItem(tabName = "df_targets", df_targetsUI("df_targets")),
        tabItem(tabName = "df_raw", df_rawUI("df_raw")),
        tabItem(tabName = "df_count", df_countUI("df_count")),
        tabItem(tabName = "df_degcount", df_degcountUI("df_degcount")),
        tabItem(tabName = "df_edgeR", df_edgeRUI("df_edgeR")),
        ## vs plots
        tabItem(tabName = "plot_point", plot_pointUI("plot_point")),
        tabItem(tabName = "plot_pca", plot_pcaUI("plot_pca")),
        tabItem(tabName = "plot_box", plot_boxUI("plot_box")),
        tabItem(tabName = "plot_tsne", plot_tsneUI("plot_tsne")),
        tabItem(tabName = "plot_mds", plot_mdsUI("plot_mds")),
        tabItem(tabName = "plot_glm", plot_glmUI("plot_glm")),
        tabItem(tabName = "plot_heat", plot_heatUI("plot_heat")),
        tabItem(tabName = "plot_clust", plot_clustUI("plot_clust")),
        tabItem(tabName = "plot_volcano", plot_volcanoUI("plot_volcano")),
        ## other tabs
        tabItem(tabName = "about", aboutUI("about"))
    )
)
# right side bar
rightsidebar <- rightSidebar(
    background = "light", icon = "clipboard-check", width = 400,
    rightUI("right")
)
# app main UI
mainUI <- dashboardPagePlus(header = dashboardHeader, sidebar = dashboardSidebar,
                            body =  dashboardBody, rightsidebar = rightsidebar)
# merge everything together
ui <- spsUI(mainUI)


