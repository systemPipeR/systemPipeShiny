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
        menuItem("Dashboard", tabName = "dashboard", icon = icon("sitemap")),
                # badgeLabel = "Main", badgeColor = "red",
                 # menuSubItem(text = "Targets", href = "#shiny-tab-vs_main")),
        menuItem(
            "Workflow Mangement", icon = icon("tasks"), tabName = "Workflow"
           
                 ),
        menuItem("Visualization", icon = icon("tasks"), tabName = "vs"),
        menuItem("Collection of plots", icon = icon("tasks"), tabName = "vs_main"),
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
    includeScript("www/sps.js"),
    tabItems(
        tabItem(tabName = "dashboard", dashboardUI("dashboard")),
        tabItem(tabName = "Workflow", wf_mainUI("wf_main")),
        tabItem(tabName = "Upload", uploadUI("upload")),
        tabItem(tabName = "EDA", edaUI("eda")),
        tabItem(tabName = "DEG", degUI("deg")),
        tabItem(tabName = "about", aboutUI("about")),
        tabItem(tabName = "vs", vs_listUI("vs_list")),
        tabItem(tabName = "vs_main", vs_mainUI("vs_main"))
    )
)
# right side bar
rightsidebar <- rightSidebar(
    background = "light", icon = "clipboard-check", width = 400,
    rightUI("right")
)


ui <- dashboardPagePlus(header = dashboardHeader, sidebar = dashboardSidebar,
                        body =  dashboardBody, rightsidebar = rightsidebar)


