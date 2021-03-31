## UI
admin_usersUI <- function(id){

    ns <- NS(id)
    tagList(
        tabTitle("App account management"),
        spsHr(),
        br(),
        h3("Current accounts"),
        tags$label("actions:"),
        actionButton(
            ns("new_user"), "Add new users", icon("users"),
            class = "btn-success", style = "color: white;"),
        spsHr(),
        DT::DTOutput(ns("acc_table"))
    )
}

## server
admin_usersServer <- function(id, shared){
    userBtn <- function(id, type) {
        if (type == "remove") {
            btn_icon <- icon("trash")
            btn_class <- "btn-danger"
            title <- "remove user"
            onclick <- "Shiny.setInputValue('admin-users-remove_user', this.id,  {priority: 'event'})"
        }
        if (type == "change_pass") {
            btn_icon <- icon("key")
            btn_class <- "btn-primary"
            title <- "change password"
            onclick <- "Shiny.setInputValue('admin-users-change_pass', this.id,  {priority: 'event'})"
        }
        if (type == "change_role") {
            btn_icon <- icon("user-tag")
            btn_class <- "btn-primary"
            title <- "change role"
            onclick <- "Shiny.setInputValue('admin-users-change_role', this.id,  {priority: 'event'})"
        }
        actionButton(
            id, "", icon = btn_icon,
            class = btn_class,
            `data-toggle` = "tooltip",
            `data-title` = title,
            `data-container` = "body",
            onclick = onclick,
            style = "color: white; float: right;"
        ) %>%
            htmltools::doRenderTags()
    }

    module <- function(input, output, session){
        ns <- session$ns
        db <- quiet(spsAccount$new())
        observeEvent(1, once = TRUE, {
            shared$admin$users$acc_event <- 0
            shared$admin$current_user <- "admin"
        })
        # render and update user table---------
        accs <- reactive({
            shared$admin$users$acc_event
            df <- db$accList()
            df[['Change role']] <- lapply(df$account, function(x) userBtn(ns(paste0("rolebtn-", x)), "change_role"))
            df[['Change password']] <- lapply(df$account, function(x) userBtn(ns(paste0("passbtn-", x)), "change_pass"))
            df[['Delete']] <- lapply(df$account, function(x) userBtn(ns(paste0("delbtn-", x)), "remove"))
            df
        })

        proxy <- DT::dataTableProxy("acc_table")
        observe(DT::replaceData(proxy, data = accs()))

        output$acc_table <- DT::renderDT({

            DT::datatable(
                isolate(accs()),
                options = list(processing = FALSE),
                escape = FALSE
            )
        })

        # delete users ----------
        remove_user <- reactiveVal()
        observeEvent(input$remove_user, {

            req(input$remove_user)
            remove_user(stringr::str_remove(input$remove_user, "^.*-"))
            if (remove_user() == shared$admin$current_user) {
                shinyCatch(stop("You cannot kill yourself"), blocking_level = "error")
            }
            shinyWidgets::confirmSweetAlert(
                session, ns("confirm_delete"), title = "Confirm account deletion",
                text = glue("You really want to delete '{remove_user()}'?"),
                type = "warning"
            )

        })

        observeEvent(input$confirm_delete, ignoreInit = TRUE, {
            req(input$confirm_delete)
            shinyCatch(db$accRemove(remove_user()), blocking_level = "error")
            shared$admin$users$acc_event <- isolate(shared$admin$users$acc_event) + 1
            shinytoastr::toastr_success("Account removed", position = "bottom-right", timeOut = 2000)
        })

        # change password ----------
        pass_user <- reactiveVal()
        observeEvent(input$change_pass, {

            req(input$change_pass)
            pass_user(stringr::str_remove(input$change_pass, "^.*-"))
            shinyWidgets::confirmSweetAlert(
                session, ns("confirm_pass"), title = glue("New password for '{pass_user()}'"),
                text = div(
                        style = "text-align: left; width:100%",
                        p("At least 8 characters including at least 1 special character."),
                        passwordInput(ns("pass_first"), label = "New password", width = "100%"),
                        passwordInput(ns("pass_second"), label = "Type it again", width = "100%"),
                    ),
                html = TRUE, type = "info"
            )

        })

        observeEvent(input$confirm_pass, ignoreInit = TRUE, {
            req(input$confirm_pass)
            if (input$pass_first != input$pass_second) shinyCatch(stop("Two passwords not matching"), blocking_level = "error")

            shinyCatch(db$accPassChange(pass_user(), input$pass_first), blocking_level = "error")
            shinytoastr::toastr_success("Password changed", position = "bottom-right", timeOut = 2000)
        })

        # change role ----------
        role_user <- reactiveVal()
        observeEvent(input$change_role, {

            req(input$change_role)
            role_user(stringr::str_remove(input$change_role, "^.*-"))
            shinyWidgets::confirmSweetAlert(
                session, ns("confirm_role"), title = glue("New role for '{role_user()}'"),
                text = div(
                    style = "text-align: left; width:100%",
                    shinyWidgets::radioGroupButtons(
                        ns("new_role"), "Choose a role",
                        choices = c("user", "admin"),
                        justified = TRUE,
                        status = "primary",
                        checkIcon = list(
                            yes = icon("ok", lib = "glyphicon"))
                    )
                ),
                html = TRUE, type = "info"
            )

        })

        observeEvent(input$confirm_role, ignoreInit = TRUE, {
            req(input$confirm_role)
            df <- isolate(accs())
            if (df[df$account == role_user(), 'role'] == input$new_role)
                shinyCatch(stop("Still the same role, abort"), blocking_level = "error")

            shinyCatch(db$accRoleChange(role_user(), input$new_role), blocking_level = "error")
            shared$admin$users$acc_event <- isolate(shared$admin$users$acc_event) + 1
            shinytoastr::toastr_success("Role changed", position = "bottom-right", timeOut = 2000)
        })

        # new account ----------
        observeEvent(input$new_user, {
            shinyWidgets::confirmSweetAlert(
                session, ns("confirm_new_user"), title = glue("Create a new user"),
                text = div(
                    style = "text-align: left; width:100%",
                    p("No space or special character, no starting with a number"),
                    textInput(ns("new_acc_name"), label = "New account name", width = "100%"),
                    shinyWidgets::radioGroupButtons(
                        ns("new_acc_role"), "Choose a role",
                        choices = c("user", "admin"),
                        justified = TRUE,status = "primary",
                        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                    ),
                    p("At least 8 characters including at least 1 special character."),
                    passwordInput(ns("new_pass_first"), label = "New password", width = "100%"),
                    passwordInput(ns("new_pass_second"), label = "Type it again", width = "100%")

                ),
                html = TRUE, type = "success"
            )

        }, ignoreInit = TRUE)

        observeEvent(input$confirm_new_user, ignoreInit = TRUE, {
            req(input$confirm_new_user)
            if (input$new_pass_first != input$new_pass_second)
                shinyCatch(stop("Two passwords not matching"), blocking_level = "error")

            shinyCatch(db$accAdd(input$new_acc_name, input$new_pass_first, input$new_acc_role), blocking_level = "error")
            shared$admin$users$acc_event <- isolate(shared$admin$users$acc_event) + 1
            shinytoastr::toastr_success("New account added", position = "bottom-right", timeOut = 2000)
        })
    }
    moduleServer(id, module)
}







