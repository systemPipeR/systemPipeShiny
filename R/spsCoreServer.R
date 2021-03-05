# SPS core server functions, can only be used under SPS framework

#' SPS main server function
#' @importFrom rlang parse_expr eval_tidy
#' @importFrom shinyjs removeClass toggleClass hide show
#' @noRd
spsServer <- function(tabs, server_expr, mod_missings) {
    spsinfo("Start to create server function")
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
        # core tabs
        spsinfo("Loading core tabs server")
        core_dashboardServer("core_dashboard", shared)

        # core_rightServer("core_right", shared)
        core_canvasServer("core_canvas", shared)
        core_aboutServer("core_about", shared)
        # modules
        module_mainServer("module_main", shared)
        if(length(mod_missings[['wf']]) == 0) {
            spsinfo("Loading workflow module server"); wfServer("wf", shared)
            core_topServer("core_top", shared) # top is now part of workflow module
        }
        if(length(mod_missings[['rna']]) == 0) {
            spsinfo("Loading core RNAseq module server"); vs_rnaseqServer("vs_rnaseq", shared)
        }
        if(length(mod_missings[['ggplot']]) == 0) {
            spsinfo("Loading ggplot module server"); vs_esqServer("vs_esq", shared)
        }

        # VS tabs
        spsinfo("Loading vs tabs server")
        vs_mainServer("vs_main", shared)
        mapply(function(module, name){
            spsinfo(glue("Loading server for {name}"))
            rlang::eval_tidy(module)
        },
            SIMPLIFY = FALSE,
            module = tab_modules,
            name = names(tab_modules))

        # global server logic, usually no need to change below
        ## pushbar set up
        ## loading screening
        spsinfo("Add loading screen logic")
        serverLoadingScreen(input, output, session)
        ## for workflow control panel
        spsinfo("Reslove workflow tabs progress tracker")
        shinyjs::removeClass(id = "wf-panel", asis = TRUE, class = "tab-pane")
        spsinfo("Loading other logic...")
        observeEvent(input$left_sidebar, {
            shinyjs::toggleClass(
                id = "wf-panel",
                class = "shinyjs-hide",
                asis = TRUE,
                condition = !str_detect(input$left_sidebar, "^wf_"))
        })

        # spsWarnings(session)
        # TODO admin page, come back in next release
        spsinfo("Loading admin panel server")
        admin_url <- reactive({
            names(getQueryString())
        })
        observe({
            req(spsOption('admin_page'))
            req(admin_url() == spsOption('admin_url'))
            shinyjs::hide("page_user", asis = TRUE)
            shinyjs::show("page_admin", asis = TRUE)
            output$page_admin <- renderUI(adminUI())
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
spsWarnings <- function(session){
    sps_warnings <- list()
    # if(spsOption('eg_tab')) {
    #     msg("Developer mode is on. you shouldn't deploy app with this mode",
    #         "SPS-DANGER", "red")
    #     sps_warnings[['eg_tab']] <- h4("You are on developer mode")
    # }
    if(getQueryString() == "admin"){
        msg("You admin page url is default, consider to change it",
            "SPS-DANGER", "red")
        sps_warnings[['admin']] <- h4("Change default admin page url")
    }

    if(spsOption('warning_toast')){
        shinyWidgets::sendSweetAlert(session = session, html = TRUE,
                       '<p style="color:var(--danger)">DANGER</p>',
                       div(class = "sps-warning",
                           tagList(sps_warnings)
                       ),
                       type = "error"
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
