adminUI <- function(){
    ns <- NS("admin")
    dashboardPagePlus(
        title = "Admin Panel",
        header = dashboardHeader(

        ),
        sidebar = dashboardSidebar(

        ),
        body =  dashboardBody(
            HTML('<h1>Developing<span class="label label-default bg-olive">Next Release</span></h1>'),
            p("Coming in next release"),
            actionButton("reload", "test button"),
            materialSwitch(
                inputId = "change",
                label = "Some switch",
                value = TRUE,
                status = "success",
                right = TRUE
            )
        )
    )
}
