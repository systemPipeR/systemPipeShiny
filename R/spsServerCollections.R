################## A Collections of server utilities############################
# Can be used in other shiny projects, no need to use under SPS framework
## use on top of shiny

# spsDebounce <- function(value, millis=1500){
#     shiny::debounce(function(){value}, millis)
# }


waitInput <- function(expr, session = getDefaultReactiveDomain()) {
    observeEvent(once = TRUE, reactiveValuesToList(session$input), {
        print("loaded")
        force(expr)
    }, ignoreInit = TRUE)
}
