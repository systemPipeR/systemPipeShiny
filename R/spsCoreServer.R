# SPS core server functions, can only be used under SPS framework

#' SPS main server function
#' @importFrom pushbar setup_pushbar
#' @importFrom rlang parse_expr eval_tidy
#' @importFrom shinyjs removeClass toggleClass hide show
#' @noRd
spsServer <- function(tabs, server_expr) {
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
        core_topServer("core_top", shared)
        # core_rightServer("core_right", shared)
        core_canvasServer("core_canvas", shared)
        core_aboutServer("core_about", shared)
        # WF tabs server
        spsinfo("Loading core workflow tabs server")
        wfServer("wf", shared)
        # VS tabs
        spsinfo("Loading vs tabs server")
        vs_mainServer("vs_main", shared)
        # user modules
        mapply(function(module, name){
            spsinfo(glue("Loading server for {name}"))
            rlang::eval_tidy(module)
        },
        SIMPLIFY = FALSE,
        module = tab_modules,
        name = names(tab_modules))

        # global server logic, usually no need to change below
        ## pushbar set up
        spsinfo("Add push bar")
        pushbar::setup_pushbar()
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
        shared$wf_flags <- data.frame(targets_ready = FALSE,
                                      wf_ready = FALSE,
                                      wf_conf_ready = FALSE)
        output$wf_panel <- wfProgressPanel(shared)
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

    # observeEvent(input$reload, ignoreInit = TRUE, {
    #     sps_options <- getOption('sps')
    #     sps_options[['loading_screen']] = isolate(input$change)
    #     options(sps = sps_options)
    #     server_file <- readLines("server.R", skipNul = FALSE)
    #     server_file[3] <-
    #         glue("# last change date: {format(Sys.time(), '%Y%m%d%H%M%S')}")
    #     writeLines(server_file, "server.R")
    #     ui_file <- readLines("ui.R", skipNul = FALSE)
    #     ui_file[3] <-
    #         glue("# last change date: {format(Sys.time(), '%Y%m%d%H%M%S')}")
    #     writeLines(ui_file, "ui.R")
    # })
        spsinfo("Loading user defined expressions")
        # additional user expressions
        rlang::eval_tidy(server_expr)
        appLoadingTime()
    }
}


#' Workflow Progress tracker server logic
#' @description use it on the top level server not inside a module. Only
#' designed for workflow tabs. For other purpose, use `pgPaneUI`.
#' @param shared the shared object
#' @importFrom shinydashboardPlus timelineBlock timelineItem timelineLabel
#' @importFrom shinyWidgets progressBar
#' @return reactive renderUI object
#' @seealso [pgPaneUpdate()]
#' @noRd
# @examples
# wfProgressPanel(shared)
wfProgressPanel <- function(shared){
    renderUI({
        total_progress <- sum(as.numeric(shared$wf_flags))/0.03
        shinydashboardPlus::timelineBlock(reversed = FALSE,
                      shinydashboardPlus::timelineItem(
                          title = "Targets file",
                          icon = timeline_icon(shared$wf_flags$targets_ready),
                          color = timeline_color(shared$wf_flags$targets_ready),
                          border = FALSE,
                          shinyWidgets::progressBar(
                              "pg_target",  striped = TRUE, status = "primary",
                              timeline_pg(shared$wf_flags$targets_ready))
                      ),
                      shinydashboardPlus::timelineItem(
                          title = "Workflow Rmd",
                          icon = timeline_icon(shared$wf_flags$wf_ready),
                          color = timeline_color(shared$wf_flags$wf_ready),
                          border = FALSE,
                          shinyWidgets::progressBar(
                              "pg_rmd", striped = TRUE, status = "primary",
                              timeline_pg(shared$wf_flags$wf_ready))
                      ),
                      shinydashboardPlus::timelineItem(
                          title = "Config",
                          icon = timeline_icon(shared$wf_flags$wf_conf_ready),
                          color = timeline_color(shared$wf_flags$wf_conf_ready),
                          border = FALSE,
                          shinyWidgets::progressBar(
                              "pg_config", striped = TRUE, status = "primary",
                              timeline_pg(shared$wf_flags$wf_conf_ready))
                      ),
                      shinydashboardPlus::timelineLabel("Ready",
                                    color = if(all(as.logical(shared$wf_flags)))
                                        "olive"
                                    else "orange"),
                      div(style = "margin-left: 60px; margin-right: 15px;",
                          shinyWidgets::progressBar(
                              "pg_wf_all", total_progress, striped = TRUE,
                              status = timline_pg_status(total_progress)
                          )
                      )
        )
    })
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


#' Load tabular files as tibbles to server
#' @description load a file to server end. It's designed to be used with the
#' input file source switch button(see it in a SPS new template data tab).
#' It uses [vroom::vroom] to load the file. In SPS, this
#' function is usually combined as downstream of  [dynamicFileServer()]
#' function on on the server side to
#' read the file into R. This loading function only works for parsing
#' tabular data, use [vroom::vroom()] internally.
#' @param choice where this file comes from, from 'upload' or example 'eg'?
#' @param data_init a tibble to return if `upload_path` or `eg_path` is not
#' provided. Return a 8x8 empty tibble if not provided
#' @param upload_path when `choice` is "upload", where to load the file, will
#' return `data_init` if this param is not provided
#' @param eg_path when `choice` is "eg", where to load the file, will
#' return `data_init` if this param is not provided
#' @param comment comment characters when load the file,
#' see help file of `vroom`
#' @param delim delimiter characters when load the file,
#' see help file of `vroom`
#' @param col_types columns specifications, see help file of `vroom`
#' @param ... other params for vroom, see help file of `vroom`
#' @details This is function is wrapped by the [shinyCatch()] function, so it
#' will show loading information both on console and on UI. This function
#' prevents errors to crash the Shiny app, so any kind of file upload will not
#' crash the app. To show message on UI, [useSps()] must be used in Shiny UI
#' function, see examples.
#' @return returns a tibble or `NULL` if parsing is unsuccessful
#' @export
#' @importFrom shinyAce is.empty
#' @importFrom tibble as_tibble
#' @importFrom vroom vroom
#' @examples
#' if(interactive()){
#'     library(shinyWidgets)
#'     # change value to 'local' to see the difference
#'     spsOption("mode", value = "server")
#'     ui <- fluidPage(
#'         useSps(),
#'         shinyWidgets::radioGroupButtons(
#'             inputId = "data_source", label = "Choose your data file source:",
#'             selected = "upload",
#'             choiceNames = c("Upload", "Example"),
#'             choiceValues = c("upload", "eg")
#'         ),
#'         dynamicFile("data_path", label = "input file"),
#'         dataTableOutput("df")
#'     )
#'
#'     server <- function(input, output, session) {
#'         tmp_file <- tempfile(fileext = ".csv")
#'         write.csv(iris, file = tmp_file)
#'         upload_path <- dynamicFileServer(input, session, "data_path")
#'         data_df <- reactive({
#'             loadDF(choice = input$data_source,
#'                    upload_path = upload_path()$datapath,
#'                    delim = ",", eg_path = tmp_file)
#'         })
#'         output$df <- renderDataTable(data_df())
#'     }
#'     shinyApp(ui, server)
#' }
loadDF <- function(choice, data_init=NULL, upload_path=NULL, eg_path=NULL,
                   comment = "#", delim = "\t",
                   col_types = vroom::cols(), ...){
    df <- shinyCatch({
        choice <- match.arg(choice, c("upload", "eg"))
        data_init <-
            if(shinyAce::is.empty(data_init)) {
                data.frame(matrix("", 8,8), stringsAsFactors = FALSE) %>%
                    tibble::as_tibble()
            }
        else {data_init}
        if(!any(class(data_init) %in% c("tbl_df", "tbl", "data.frame"))) {
            stop("data_init need to be dataframe or tibble")
        }
        data_init <- tibble::as_tibble(data_init)
        if (choice == "upload" & shinyAce::is.empty(upload_path))
            return(data_init)
        if (choice == "eg" & shinyAce::is.empty(eg_path))
            return(data_init)
        upload_path <- switch(choice,
                              "upload" = upload_path,
                              "eg" = eg_path,
        )
        df <- shinyCatch(vroom::vroom(
            upload_path,  delim = delim, comment = comment,
            col_types = col_types, ...
        ))
        if(is.null(df)){msg("Can't read file, return empty", "error")}
        if(!names(df) %>% validUTF8() %>% all()){
            msg("non UTF-8 coding detected, return empty", "error")}
        df
    })
    return(df)
}

#' Validate expressions
#' @description this function is usually used on server side to validate input
#' dataframe or some expression
#' @param expr the expression to validate data or other things. It should
#' return `TRUE` if pass or use `stop("your message")` if fail. Other types of
#' return are acceptable but not recommended. As long as it is not empty or
#' `FALSE` by the `emptyIsFalse()` function, it will return `TRUE`
#' @param vd_name validate title
#' @param pass_msg string, if pass, what message do you want to show
#' @param fail_msg string, optional, if your expression does not contain the
#' use of `stop()`  for failure and only returns `FALSE` or other empty values,
#' this message will show and generate `shiny reactive stop`
#' @param shiny you can use this function outside shiny,
#' see `shinyCatch()` for more
#' @param verbose bool, show pass message? Default follows project verbose
#' setting
#' @seealso \code{\link{shinyCatch}}, \code{\link{emptyIsFalse}}
#' @return If expression returns empty or `FALSE` make it `shiny reactive stop`
#' and no final return, else `TRUE`.
#' @export
#'
#' @examples
#' spsOption("verbose", TRUE)
#' if(interactive()){
#'     ui <- fluidPage(
#'         useSps(),
#'         actionButton("vd1", "validate1"),
#'         actionButton("vd2", "validate2")
#'     )
#'     server <- function(input, output, session) {
#'         mydata <- datasets::iris
#'         observeEvent(input$vd1, {
#'             spsValidate({
#'                 is.data.frame(mydata)
#'             }, vd_name = "Is df")
#'             print("continue other things")
#'         })
#'         observeEvent(input$vd2, {
#'             spsValidate({
#'                 nrow(mydata) > 200
#'             }, vd_name = "more than 200 rows")
#'             print("other things blocked")
#'         })
#'     }
#'     shinyApp(ui, server)
#' }
#' # outside shiny example
#' mydata2 <- list(a = 1, b = 2)
#' spsValidate({(mydata2)}, "Not empty", shiny = FALSE)
#' try(spsValidate(is.data.frame(mydata2),
#'                 "is dataframe?",
#'                 shiny = FALSE),
#'     silent = TRUE)
spsValidate <- function(expr,
                        vd_name="validation",
                        pass_msg = glue("{vd_name} passed"),
                        fail_msg = glue("{vd_name} failed"),
                        shiny = TRUE,
                        verbose = spsOption('verbose')){
    result <- shinyCatch(expr,
                         blocking_level = "error",
                         shiny = shiny) %>% emptyIsFalse()
    if(result){
        if(verbose){
            spsinfo(pass_msg, TRUE)
            if(shiny){
                shinytoastr::toastr_success(
                    pass_msg, position = "bottom-right", timeOut = 3000)
            }
        }
        return(TRUE)
    } else {
        shinyCatch(stop(fail_msg), blocking_level = 'error', shiny = shiny)
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
