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
            includeCSS("www/sps.css"),
            includeScript("www/sps.js"),
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
                ## vs df add to sidebar
                menuSubItem(text = "Raw data", tabName = "df_raw"),
                menuSubItem(text = "xx1 data", tabName = "df_xx1"),
                menuSubItem(text = "xx2 data", tabName = "df_xx2")
                ),
            menuItem(
                text = "Collection of plots",
                ## vs plot add to sidebar
                menuSubItem(text = "Scatter Plot", tabName = "plot_point"),
                menuSubItem(text = "PCA Plot", tabName = "plot_pca"),
                menuSubItem(text = "xx2 Plot", tabName = "plot_xx1"),
                menuSubItem(text = "xx3 Plot", tabName = "plot_xx1")
                )
        ),
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
        ## vs df
        tabItem(tabName = "df_raw", df_rawUI("df_raw")),
        ## vs plots
        tabItem(tabName = "plot_point", plot_pointUI("plot_point")),
        tabItem(tabName = "plot_pca", plot_pcaUI("plot_pca")),
        # other tabs
        tabItem(tabName = "about", aboutUI("about"))
    )
)
# right side bar
rightsidebar <- rightSidebar(
    background = "light", icon = "clipboard-check", width = 400,
    rightUI("right")
)
# merge everything together
ui <- dashboardPagePlus(header = dashboardHeader, sidebar = dashboardSidebar,
                        body =  dashboardBody, rightsidebar = rightsidebar)


