################## A Collections of server utilities############################

## use on top of shiny

# library(shiny)
# library(toastr)

# display the error, warning, message text by a toastr bar on the app
# position = c("top-right", "top-center", "top-left",
# "top-full-width", "bottom-right", "bottom-center", "bottom-left",
# "bottom-full-width")
shinyCatch <- function(expr, position = "bottom-right") {
    tryCatch(
        {expr},
        error = function(e) {
            toastr_error(
                message = e$message, position = position, closeButton = TRUE, timeOut = 0, 
                title = "There is an error", hideDuration = 300,
                    )
        },
        warning = function(w) {
            toastr_warning(message = w$message, position = position, closeButton = TRUE, timeOut = 5000)
        },
        message = function(m) {
            toastr_info(message = m$message, position = position, closeButton = TRUE, timeOut = 3000)
        })
}
# @example
# ui <- fluidPage(
#     useToastr(),
#     actionButton("btn1","Click me1"),
#     actionButton("btn2","Click me2"),
#     actionButton("btn3","Click me3"),
#     
#     textOutput("text")
# )
# server <- function(input, output, session) {
#     observeEvent(input$btn1, {
#         shinyCatch(stop("stop"))
#     })
#     observeEvent(input$btn2, {
#         shinyCatch(warning("warning"))
#     })
#     observeEvent(input$btn3, {
#         shinyCatch(message("message"))
#     })
# }
# shinyApp(ui, server)

