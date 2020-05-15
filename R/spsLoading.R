#################### SPS loading screen functions ######################
# Use this on UI
renderLoading <- function(){
    switch(getOption("sps")$loading_theme %>% as.character(),
           "vhelix" = loadingVhelix(),
           loading404()
    )
}
# Use this on server
serverLoadingScreen <- function(input, output, session){
    observeEvent(input$toapp,{
        shinyjs::hide("loading-screen", anim = TRUE, animType = "fade")
        shinyjs::show("app-main", anim = TRUE, animType = "fade")
    })
}


#### loading screen collections
loading404 <- function(){
    includeHTML("www/nyan.html")
}

loadingVhelix <- function(){
    tagList(
        includeHTML("www/vhelix.html"),
        particlesjs::particles(
            target_id ="loading-screen",
            element_id = "particles",
            config = "www/particlesjs-config.json"
        )
    )
}
