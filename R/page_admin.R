#' Admin page UI
#' Internal func
#' @importFrom shinydashboard dashboardSidebar dashboardBody
#' @importFrom shinydashboardPlus dashboardPagePlus dashboardHeaderPlus
#' @importFrom shinyWidgets materialSwitch
#' @noRd
adminUI <- function(){
    ns <- NS("admin")
    shinydashboardPlus::dashboardPagePlus(
        title = "Admin Panel",
        header = shinydashboardPlus::dashboardHeaderPlus(

        ),
        sidebar = shinydashboard::dashboardSidebar(

        ),
        body =  shinydashboard::dashboardBody(
            HTML('<h1>Developing<span class="label label-default
                 bg-olive">Next Release</span></h1>'),
            p("Coming in next release"),
            actionButton("reload", "test button"),
            shinyWidgets::materialSwitch(
                inputId = "change",
                label = "Some switch",
                value = TRUE,
                status = "success",
                right = TRUE
            )
        )
    )
}


# TODO toggle sps options
# TODO visitor stats
# TODO encrypt, decrypt file
# TODO
