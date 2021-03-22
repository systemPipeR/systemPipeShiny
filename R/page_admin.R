
spsUIadmin <- function(){
    div(
        class = "sps-admin",
        adminLoginUI(),
        tags$head(
            tags$script(src="sps/js/sps_admin.js"),
            tags$script(src="sps/js/micro.js"),
            tags$link(rel="stylesheet", href = "sps/css/sps_admin.css")
        ),
        uiOutput(
            outputId = "page_admin", container = div,
            style = "margin-right: -2em; height:auto;",
            class = "shinyjs-hide skin-blue"
        )
    )
}


adminServer <- function(input, output, session, shared) {
    observeEvent(1, once = TRUE, {
        shared$admin$log_success <- FALSE
    })
    adminLoginServer("admin", shared)
    observeEvent(shared$admin$log_success, {
        req(isTRUE(shared$admin$log_success))
        shinyjs::hide("admin-login_page", asis = TRUE)
        output$page_admin <- renderUI(adminUI())
        shinyjs::show("page_admin", asis = TRUE)
        output$page_admin <- renderUI(adminUI())
        shinyjs::runjs("$('body').trigger('admin-displayed')")
        admin_infoServer("admin-info", shared)
        admin_usersServer("admin-users", shared)
    })
}





#' Admin page UI
#' Internal func
#' @importFrom shinyWidgets materialSwitch
#' @noRd
adminUI <- function(){
    ns <- NS("admin")
    shinydashboardPlus::dashboardPage(
        title = "Admin",
        header = shinydashboard::dashboardHeader(
            title = tagList(
                span(class = "logo-lg", "Admin Panel"),
                img(src = spsOption('title_logo'), height = "25", width = "25")
            )
        ),
        sidebar = shinydashboardPlus::dashboardSidebar(
            br(),
            shinydashboard::sidebarMenu(
                id = ns(id = "left_sidebar"),
                shinydashboard::menuItem("General info", icon = icon("server"), tabName = ns("info")),
                shinydashboard::menuItem("Users", icon = icon("users"), tabName = ns("users"))
            )
        ),
        body =  shinydashboard::dashboardBody(
            class = "sps",
            tags$head(

            ),
            spsComps::spsGoTop(),
            shinydashboard::tabItems(
                shinydashboard::tabItem(tabName = ns("info"), admin_infoUI(ns("info"))),
                shinydashboard::tabItem(tabName = ns("users"), admin_usersUI(ns("users")))
            )
        )
    )
}


# TODO toggle sps options
# TODO visitor stats
# TODO encrypt, decrypt file
# TODO
