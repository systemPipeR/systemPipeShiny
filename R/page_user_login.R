userLoginUI <- function(login_message){
    ns <- NS("user")
    div(
        id = ns("login_page"),
        uiOutput(ns("bg")),
        div(
            class= "sps-blink",
            shinyWidgets::actionBttn(
                inputId = ns("toapp"), "Login",
                icon = icon("angle-double-right"),
                size = "lg"
            )
        ),
        div(
            id = ns("theme-container"),
            h4("Random theme everytime, choose more", icon("caret-down")),
            selectizeInput(
                ns("choose_theme"),
                label = "",
                width = "100%",
                choices = list(
                    `double helix` ="vhelix",
                    `DNA flow`= "hhelix",
                    `bio-matrix` = "biomatrix",
                    emtpy = "empty"
                ),
                options = list(
                    placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            )
        ),
        div(
            class = "login",
            id = ns("login"),
            style = "display: none;",
            div(
                class = "img-container",
                tags$img(src="img/logo.png")
            ),
            div(
                class = "login-box form-control",
                div(login_message, class="text-center"),
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
                    icon("triangle-exclamation"),
                    tags$label("Incorrect username or password")
                )
            )
        )
    )
}


userLoginServer <- function(id, shared) {
    module <- function(input, output, session) {
        attempt <- reactiveVal(0)
        theme <- spsOption("login_theme")
        output$bg <- renderUI({
            renderLoading("404")
        })
        if (!emptyIsFalse(theme) || theme == "random")
            theme <- sample(c("vhelix", "hhelix", "biomatrix"), 1)
        updateSelectInput(session, "choose_theme", selected = theme)
        observeEvent(input$choose_theme, {
            output$bg <- renderUI({
                session$sendCustomMessage("sps-loading-bg", message = list(bg = input$choose_theme))
                renderLoading(isolate(input$choose_theme))
            })
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        observeEvent(input$login_click, {
            shinyjs::hide("login-alert")
            req(input$login_uname)
            req(input$login_pass)
            req(attempt() < 3)
            res <- shared$db$accMatch(input$login_uname, input$login_pass)
            if (res) {
                shared$user$log_success <- TRUE
            } else {
                shared$user$log_success <- FALSE
                spsinfo(glue("Fail login attempt with username: {input$login_uname}"))
                attempt(isolate(attempt()) + 1)
                shinyjs::show("login-alert")
            }
        }, ignoreInit = TRUE)

        observeEvent(attempt(), {
            req(attempt() >= 3)
            shinyCatch(stop("Too many fail attempts."))
            shinyjs::runjs('$("#user-login .login-box").remove()')
        })


    }
    moduleServer(id, module)
}
