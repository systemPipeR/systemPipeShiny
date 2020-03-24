####### UI 
## valid colors: 
## red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

# css
btn_style <- "color: #fff; background-color: #337ab7; border-color: #2e6da4" # buttons, default is too ugly, see UI.R
widget_user_style <- "overflow-y:auto"

# header
dashboardHeader <- dashboardHeaderPlus(
    title = tagList(
        span(class = "logo-lg", "systemPipeShiny"), 
        img(src = "systemPipe_small.png"),
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
        menuItem("Dashboard", tabName = "dashboard", icon = icon("sitemap"),
                 badgeLabel = "Main", badgeColor = "red"),
        menuItem("Workflow Mangement", icon = icon("tasks"), tabName = "Workflow"),
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
    useToastr(),
    tags$style(
        glue('
             .btn-default {
               @{btn_style}@;
             }
             .btn-default:hover{
               background-color: #4c92cf;
             }
             .widget-user {
               @{widget_user_style}@;
             }
             ', .open = "@{", .close = "}@")
    ),
    tabItems(
        tabItem(tabName = "dashboard", dashboardUI("dashboard")),
        tabItem(tabName = "Workflow", wf_mainUI("wf_main")),
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


