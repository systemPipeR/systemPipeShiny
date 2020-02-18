####### UI 
## valid colors: 
## red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

# header
dashboardHeader <- dashboardHeaderPlus(
    title = "SystemPipeR",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "clipboard-check", 
    left_menu = topUI("top")
)
# side bar
dashboardSidebar <-  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("sitemap"),
                 badgeLabel = "Main", badgeColor = "red"),
        menuItem("Targets", icon = icon("tasks"), tabName = "Targets"),
        menuItem("Workflow", icon = icon("tasks"), tabName = "Workflow"),
        menuItem("Upload", icon = icon("tasks"), tabName = "Upload"),
        menuItem("Exploratory Data Analysis", icon = icon("tasks"), tabName = "EDA"),
        menuItem("DEG Analysis", icon = icon("tasks"), tabName = "DEG"),
        menuItem("About", icon = icon("info"), tabName = "about")
    )
)
# body
dashboardBody <- dashboardBody(
    useShinyjs(),
    useSweetAlert(),
    tabItems(
        tabItem(tabName = "dashboard", dashboardUI("dashboard")),
        tabItem(tabName = "Targets", targetUI("targets")),
        tabItem(tabName = "Workflow", wfUI("wf")),
        tabItem(tabName = "Upload", uploadUI("upload")),
        tabItem(tabName = "EDA", edaUI("eda")),
        tabItem(tabName = "DEG", degUI("deg")),
        tabItem(tabName = "about", aboutUI("about"))
    )
)
# right side bar
rightsidebar <- rightSidebar(
    background = "light", icon = "clipboard-check", width = 400,
    rightUI("right")
)


ui <- dashboardPagePlus(header = dashboardHeader, sidebar = dashboardSidebar,
                        body =  dashboardBody, rightsidebar = rightsidebar)


