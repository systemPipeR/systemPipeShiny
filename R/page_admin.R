

spsUIadmin <- function(){
    spsinfo("Loading admin page UI")
    div(
        class = "sps-page",
        id = "page-admin-wrapper",
        adminLoginUI(),
        tags$head(
            tags$script(src="sps/js/sps_admin.js"),
            tags$script(src="sps/js/micro.js"),
            tags$link(rel="stylesheet", href = "sps/css/sps_login.css")
        ),
        uiOutput(
            outputId = "page_admin", container = div,
            class = "shinyjs-hide skin-blue"
        )
    )
}


adminServer <- function(input, output, session, shared) {
    observeEvent(1, once = TRUE, {
        shared$admin$log_success <- FALSE
        shared$admin$ui_loaded <- FALSE
    })
    ui_sent <- reactiveVal(FALSE)
    spsinfo("Loading admin page server")
    adminLoginServer("admin", shared)
    observeEvent(shared$admin$log_success, {
        req(isTRUE(shared$admin$log_success))
        req(isFALSE(ui_sent()))
        shinyjs::runjs('$("#admin-login_page").remove();')
        output$page_admin <- renderUI(adminUI())
        waitInput({shared$admin$ui_loaded <- TRUE})
        shinyjs::show("page_admin", asis = TRUE, anim = TRUE)
        shinyjs::runjs("$('body').trigger('admin-displayed')")
        ui_sent(TRUE)
    })

    observeEvent(shared$admin$ui_loaded, {
        req(isTRUE(shared$admin$ui_loaded))
        req(isTRUE(shared$admin$log_success))
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
            class = "",
            tags$head(
                tags$link(rel="stylesheet", href = "sps/css/sps_admin.css")
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
