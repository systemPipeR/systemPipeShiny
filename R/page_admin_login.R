adminLoginUI <- function(){
    ns <- NS("admin")
    div(
        id = ns("login_page"),
        div(id = ns("bg")),
            tags$label(
                class = "about-micro",
                "About the background: ",
            tags$a(href="https://codepen.io/plasm/details/YVKZgd", target="_blank", "PLANKTON")
        ),
        div(
            class = "login",
            id = ns("login"),
            div(
                class = "login-box form-control",
                h3("Admin Login", class="text-center"),
                div(
                    class = "uname",
                    textInput(
                        inputId = ns("login_uname"),
                        label = "Username",
                        width = "100%",
                        placeholder="Enter Username"
                    )
                ),
                div(
                    class = "pass",
                    passwordInput(
                        inputId = ns("login_pass"),
                        label = "Password",
                        width = "100%"
                    )
                ), br(),
                actionButton(ns("login_click"), "Login", width = "100%", class = "btn-info"),
                div(
                    id = ns("login-alert"),
                    class = "alert alert-danger",
                    style = "display: none",
                    icon("exclamation-triangle"),
                    tags$label("Incorrect username or password, or no admin access")
                )

            )
        )
    )
}


adminLoginServer <- function(id, shared) {
    module <- function(input, output, session) {
        attempt <- reactiveVal(0)
        observeEvent(input$login_click, {
            shinyjs::hide("login-alert")
            req(input$login_uname)
            req(input$login_pass)
            req(attempt() < 3)
            res <- shared$db$accMatch(input$login_uname, input$login_pass, role = "admin", match_role = TRUE)
            if (res) {
                shared$admin$log_success <- TRUE
            } else {
                shared$admin$log_success <- FALSE
                spsinfo(glue("Fail login attempt with username: {input$login_uname}"))
                attempt(isolate(attempt()) + 1)
                shinyjs::show("login-alert")
            }
        }, ignoreInit = TRUE)

        observeEvent(attempt(), {
            req(attempt() >= 3)
            shinyCatch(stop("Too many fail attempts."))
            shinyjs::runjs('$("#admin-login .login-box").remove()')
        })


    }
    moduleServer(id, module)
}


