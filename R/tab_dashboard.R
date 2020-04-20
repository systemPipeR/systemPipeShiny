## UI
dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        h2("Dashboard"), 
        tags$p(strong("This page controls targets file and configuration of other parameters.")),
        p("If you cannot click some buttons, that means they are disabled at current tab or you need to do other things first, e.g. upload a file."),
        
        fluidRow(
            carousel(width = 12,
                id = "dashcarousel",
                carouselItem(
                    caption = "SPS",
                    tags$img(src = "systemPipeR_site.png")
                ),
                carouselItem(
                    caption = "Preview",
                    tags$img(src = "sps_wf.png")
                )
            )
        ),

    )
}

## server
dashboardServer <- function(input, output, session){
    
}
