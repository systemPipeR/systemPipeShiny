# SPS core server functions, can only be used under SPS framework

#' SPS main server function
#' @importFrom rlang parse_expr eval_tidy
#' @importFrom shinyjs removeClass toggleClass hide show
#' @noRd
spsServer <- function(tabs, server_expr, mod_missings, sps_env, guide, mainUI) {
    spsinfo("Start to create server function")
    spsinfo("Resolve default tabs server")

    spsinfo("Load custom tabs servers")
    tab_modules <- if(nrow(tabs) > 0) {
        names(tabs[['tab_id']]) <- tabs[['tab_id']]
        lapply(tabs[['tab_id']], function(x){
            glue('{x}Server("{x}", shared)') %>% rlang::parse_expr()
        })
    } else list(empty = substitute(spsinfo("No custom server to load.")))

    function(input, output, session) {
        # add a container to communicate tabs
        spsinfo("Start to load server")
        spsinfo("Creating shared object")
        shared <- reactiveValues()
        load_admin <- reactiveVal(FALSE)
        spsinfo("Creating SPS db inside shared")
        shared$db <- quiet(spsAccount$new())

        # render mainUI on login

        ## if login successful or no login required
        nologin <- reactiveVal(FALSE)
        observeEvent(1, once = TRUE, {
            if (spsOption('login_screen')) {
                spsinfo("Add login screen logic")
                userServer(input, output, session, shared, mainUI)
            } else {
                nologin(TRUE)
            }
        })

        observeEvent(c(input$userUI_loaded, nologin()), {
            req(!isTRUE(load_admin()))
            if (spsOption('login_screen')) {
                req(input$userUI_loaded)
                req(!emptyIsFalse(shared$user$server_loaded))
                req(isTRUE(shared$user$log_success))
                updateProgressBar(session, "user-pg", 50 , title = "UI loading done, start to load server", status = "info")
                on.exit({
                    shinyjs::hideElement('user-pg-panel',  asis = TRUE, anim = TRUE)
                })
            } else {
                req(isTRUE(nologin()))
            }

            # core tabs
            spsinfo("Loading core tabs server")
            if (spsOption('tab_welcome')) rlang::env_get(sps_env, 'core_welcomeServer', core_welcomeServer)("core_welcome", shared)

            any_module <- any(spsOption("module_wf"), spsOption("module_rnaseq"), spsOption("module_ggplot"))
            if (any_module) rlang::env_get(sps_env, 'module_mainServer', module_mainServer)("module_main", shared)

            if (spsOption('tab_vs_main')) {
                rlang::env_get(sps_env, 'vs_mainServer', vs_mainServer)("vs_main", shared)
                # VS tabs
                spsinfo("Loading custom tabs server")
                mapply(function(module, name){
                    spsinfo(glue("Loading server for {name}"))
                    rlang::eval_tidy(module)
                },
                SIMPLIFY = FALSE,
                module = tab_modules,
                name = names(tab_modules))
            }

            if (spsOption('tab_canvas')) rlang::env_get(sps_env, 'core_canvasServer', core_canvasServer)("core_canvas", shared)
            if (spsOption('tab_about')) rlang::env_get(sps_env, 'core_aboutServer', core_aboutServer)("core_about", shared)
            if (spsOption('login_screen')) updateProgressBar(session, "user-pg", 75 , title = "Loaed core servers", status = "primary")
            # modules
            if(!is.null(mod_missings[['wf']]) && length(mod_missings[['wf']]) == 0) {
                spsinfo("Loading workflow module server"); wfServer("wf", shared)
                core_topServer("core_top", shared) # top is now part of workflow module
            }
            if(!is.null(mod_missings[['rna']]) && length(mod_missings[['rna']]) == 0) {
                spsinfo("Loading core RNAseq module server"); vs_rnaseqServer("vs_rnaseq", shared)
            }
            if(!is.null(mod_missings[['ggplot']]) && length(mod_missings[['ggplot']]) == 0) {
                spsinfo("Loading ggplot module server"); vs_esqServer("vs_esq", shared)
            }
            if (spsOption('login_screen')) updateProgressBar(session, "user-pg", 100 , title = "Loading complete", status = "success")
            shared$user$server_loaded <- TRUE
        })

        # load guides
        if(!emptyIsFalse(checkNameSpace('cicerone', TRUE))) {
            guide_content <- guide[['guide_content']]
            guide_names <- names(guide_content)
            lapply(seq_along(guide_content), function(i) {
                observeEvent(input[[guide_names[i]]], {
                    guide_content[[i]]$init(session = session)$start(session = session)
                })
            })
            observeEvent(shared$user$server_loaded, {
                req(shared$user$server_loaded)
                cicerone::Cicerone$new(overlay_click_next =TRUE)$step(
                    ".navbar-static-top .messages-menu",
                    is_id = FALSE,
                    "Welcome",
                    "If you are new to the app, you can start with a tutorial by clicking here.
                    You can customize your own tutorials too!",
                    "left")$
                    init()$
                    start()
            })
        }



        spsinfo("Loading admin panel server functions")
        admin_url <- reactive(names(getQueryString()))
        observe({
            req(spsOption('admin_page'))
            req(admin_url() == spsOption('admin_url'))
            load_admin(TRUE)
            shinyjs::runjs('$("#page-user-wrapper").remove();')
            output$spsUIadmin <- renderUI(spsUIadmin())
            adminServer(input, output, session, shared)
        }, priority = 99)

        observeEvent(1, once = TRUE, {
            if (spsOption('warning_toast')) {
                spsinfo("check for potential risks")
                spsWarnings(session, shared)
            }
        })

        spsinfo("Loading user defined expressions")
        # additional user expressions
        rlang::eval_tidy(server_expr)
        appLoadingTime()
        # browser()
    }
}


#' Warning toast under some options when app starts
#'
#' @param session shiny session
#' @noRd
#' @return
#' @importFrom shinyWidgets sendSweetAlert
spsWarnings <- function(session, shared){
    sps_warnings <- list()
    accs <-  quiet(shared$db$accList()[['account']])
    if ('admin' %in% accs) {
        msg("The admin account is still 'admin', consider change it.",
            "SPS-DANGER", "red")
        sps_warnings[['admin']] <- tags$li("Change default admin account")
    }
    if ('user' %in% accs) {
        msg("The default user account is still 'user', consider change it.",
            "SPS-DANGER", "red")
        sps_warnings[['user']] <- tags$li("Change default user account")
    }
    if(spsOption('admin_url') == "admin"){
        msg("You admin page url is default, consider to change it",
            "SPS-DANGER", "red")
        sps_warnings[['admin_url']] <- tags$li("Change default admin page url")
    }

    if(spsOption('warning_toast')){
        shinyWidgets::sendSweetAlert(
            session = session, html = TRUE,  type = "error",
            '<p style="color:var(--danger)">DANGER: Fix these issues before deployment</p>',
            tags$ul(class = "sps-warning",
                    tagList(sps_warnings)
            )
        )
    }
}


#' Print app loading time to console
#' @noRd
appLoadingTime <- function(){
    if(exists('time_start')){
        if(inherits(time_start, "POSIXct")){
            load_time <- round(Sys.time() - time_start, 3)
            spsinfo(glue("App UI server loading done in {load_time}s!"),
                    verbose = TRUE)
        }
    }
}
