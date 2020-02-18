## UI
dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        h2("Dashboard"), 
        tags$p(strong("This page controls targets file and configuration of other parameters.")),
        p("If you cannot click some buttons, that means they are disabled at current tab or you need to do other things first, e.g. upload a file.")
    )
}

## server
dashboardServer <- function(input, output, session){
    
}