
spsUIuser <- function(login_message){
    div(
        class = "sps-page",
        id = "page-user-wrapper",
        userLoginUI(login_message),
        tags$head(
            tags$script(src="sps/js/sps_user.js"),
            tags$link(rel="stylesheet", href = "sps/css/sps_login.css")
        ),
        uiOutput(
            outputId = "page_user", container = div,
            class = "shinyjs-hide skin-blue"
        ),
        absolutePanel(
            id = "user-pg-panel",
            style = "background-color: #ecf0f5; border: 2px solid #d2d6de; border-radius: 5px; display: none;",
            top = "40%",
            left = "40%",
            width = "400px",
            height = "100px",
            fixed = FALSE,
            cursor = "default",
            h4("Pleae wait, loading UI and Server", style="text-align: center"), br(),
            shinyWidgets::progressBar(
                id = "user-pg", value = 0,
                title = "", striped = TRUE,
                display_pct = TRUE
            )
        )
    )
}


userServer <- function(input, output, session, shared, mainUI) {
    observeEvent(1, once = TRUE, {
        shared$user$log_success <- FALSE
        shared$user$ui_sent <- FALSE
        shared$user$ui_loaded <- FALSE
    })
    userLoginServer("user", shared)
    observeEvent(shared$user$log_success, {
        req(isTRUE(shared$user$log_success))
        req(isFALSE(shared$user$ui_sent))
        shinyjs::hide("user-login_page", asis = TRUE, anim = TRUE)
        shinytoastr::toastr_success("Successful", position = "bottom-right", timeOut = 2000)
        shinyjs::showElement("user-pg-panel", asis = TRUE, anim = TRUE)
        updateProgressBar(session, "user-pg", 0 , title = "Loading UI", status = "danger")
        output$page_user <- renderUI(mainUI)
        updateProgressBar(session, "user-pg", 25 , title = "Send UI to client", status = "warning")
        waitInput({shared$user$ui_loaded <- TRUE})
        shinyjs::show("page_user", asis = TRUE, anim = TRUE)
        shinyjs::runjs("$('body').trigger('user-displayed')")
        shared$user$ui_sent <- TRUE
    }, ignoreInit = TRUE)
}



