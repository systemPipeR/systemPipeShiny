# SPS core server functions, can only be used under SPS framework

#' SPS main server function
#' @importFrom pushbar setup_pushbar
#' @importFrom rlang parse_expr eval_tidy
#' @importFrom shinyjs removeClass toggleClass hide show
#' @noRd
spsServer <- function(tabs, server_expr) {
    spsinfo("Start to create server function")
    tab_modules <- if(nrow(tabs) > 0) {
        vapply(tabs[['tab_id']], function(x){
            glue('{x}Server("{x}", shared)') %>% rlang::parse_expr()
        }, list(1))
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
        wf_mainServer("wf_main", shared)
        wf_targetServer("wf_targets", shared)
        wf_wfServer("wf_wf", shared)
        wf_configServer("wf_config", shared)
        wf_runServer("wf_run", shared)
        # VS tabs
        spsinfo("Loading vs tabs server")
        vs_mainServer("vs_main", shared)
        devComponents("server", shared = shared) # for templates
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
#' @seealso pgPaneUpdate
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
    if(spsOption('dev')) {
        msg("Developer mode is on. you shouldn't deploy app with this mode",
            "SPS-DANGER", "red")
        sps_warnings[['dev']] <- h4("You are on developer mode")
    }
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
#' @description load a file to server end. Designed to be used with the input
#' file source switch button. Use `vroom` to load the file. In SPS, this
#' function is usually combined with `dynamicFile()` function to help users
#' upload file and read the file. This loading function only works for parsing
#' tabular data, use `vroom()` internally.
#' @param choice where this file comes from, from 'upload' or example 'eg'?
#' @param df_init a tibble to return if `upload_path` or `eg_path` is not
#' provided. Return a 8x8 empty tibble if not provided
#' @param upload_path when `choice` is "upload", where to load the file, will
#' return `df_init` if this param is not provided
#' @param eg_path when `choice` is "eg", where to load the file, will
#' return `df_init` if this param is not provided
#' @param comment comment characters when load the file,
#' see help file of `vroom`
#' @param delim delimiter characters when load the file,
#' see help file of `vroom`
#' @param col_types columns specifications, see help file of `vroom`
#' @param ... other params for vroom, see help file of `vroom`
#'
#' @return a tibble
#' @export
#' @importFrom shinyAce is.empty
#' @importFrom tibble as_tibble
#' @importFrom vroom vroom
#' @examples
#' if(interactive()){
#'     library(shinyWidgets)
#'     ui <- fluidPage(
#'         shinyWidgets::radioGroupButtons(
#'             inputId = "data_source", label = "Choose your data file source:",
#'             selected = "upload",
#'             choiceNames = c("Upload", "Example"),
#'             choiceValues = c("upload", "eg")
#'         ),
#'         fileInput("df_path", label = "input file"),
#'         dataTableOutput("df")
#'     )
#'
#'     server <- function(input, output, session) {
#'         tmp_file <- tempfile(fileext = ".csv")
#'         write.csv(iris, file = tmp_file)
#'         data_df <- reactive({
#'             loadDF(choice = input$data_source,
#'                    upload_path = input$df_path$datapath,
#'                    delim = ",", eg_path = tmp_file)
#'         })
#'         output$df <- renderDataTable(data_df())
#'     }
#'     shinyApp(ui, server)
#' }
loadDF <- function(choice, df_init=NULL, upload_path=NULL, eg_path=NULL,
                   comment = "#", delim = "\t",
                   col_types = vroom::cols(), ...){
    df <- shinyCatch({
        choice <- match.arg(choice, c("upload", "eg"))
        df_init <-
            if(shinyAce::is.empty(df_init)) {
                data.frame(matrix("", 8,8), stringsAsFactors = FALSE) %>%
                    tibble::as_tibble()
            }
        else {df_init}
        if(!any(class(df_init) %in% c("tbl_df", "tbl", "data.frame"))) {
            stop("df_init need to be dataframe or tibble")
        }
        df_init <- tibble::as_tibble(df_init)
        if (choice == "upload" & shinyAce::is.empty(upload_path))
            return(df_init)
        if (choice == "eg" & shinyAce::is.empty(eg_path))
            return(df_init)
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

#' Validate some inputs
#' @description Useful to check if a input dataframe, list or other things
#' meet the requirement. If any validation fails, it will show a pop-up message
#' and block any further code execution, similar to `shiny::req()`
#' @param validate_list a list of function which are in a named list and only
#' returns a single value of TRUE or FALSE. this return value needs to be named.
#' This return name will be used as error message.
#' @param args a `named`(very important) list of arguments that will be used in
#' different function in  `validate_list`. `...` argument is not supported.
#' Positional args will not work either.
#' Have to specify the name of argument, position argument also is not working.
#' @param title Title of this validator
#' @return if any validation function returns FALSE, a error pop-up will show
#' in the shiny app.
#' @export
#' @importFrom rlang is_missing is_bool
#' @importFrom shinytoastr toastr_success
#' @examples
#' if(interactive()){
#'     df_validate_common <- list(
#'         vd1 = function(df, apple){
#'             print(apple)
#'             if (is(df, "data.frame")) {result <- c(" " = TRUE)}
#'             else {result <- c("Input is not dataframe or tibble" = FALSE)}
#'             return(result)
#'         },
#'         vd2 = function(df, banana, orange="orange"){
#'             print(banana)
#'             print(orange)
#'             # change < to > in next line to see failure
#'             if (nrow(df) < 10) {result <- c(" " = TRUE)}
#'             else {result <- c("Need more than 10 rows" = FALSE)}
#'             Sys.sleep(1)
#'             return(result)
#'         }
#'     )
#'     ui <- fluidPage(
#'         actionButton("btn", "this btn")
#'     )
#'     server <- function(input, output, session) {
#'         observeEvent(input$btn, {
#'             spsValidator(df_validate_common,
#'                          args = list(df = data.frame(),
#'                                      apple = "apple",
#'                                      banana = "banana"))
#'             print(123)
#'         })
#'     }
#'     shinyApp(ui, server)
#' }
spsValidator <- function(validate_list, args = list(), title = "Validation"){
    # pre checks
    if(!is.list(args)) msg("Args must be in a list", "error")
    if(!is.list(validate_list)) msg("Validate_list must be in a list", "error")
    if(any(is.null(validate_list)))
        msg("All functions need to be named", "error")
    arg_names <- names(args)
    if(any(is.null(arg_names))) msg("All args must be named", "error")
    # check if all required args are provided
    inject_args <- vapply(seq_along(validate_list), function(each_vd) {
        if(!is.function(validate_list[[each_vd]])) {
            msg("Each item in `validate_list` must be a function", "error")}
        each_vd_args <- formals(validate_list[[each_vd]])
        vapply(seq_along(each_vd_args), function(each_arg) {
            required_arg <- rlang::is_missing(each_vd_args[[each_arg]])
            if(required_arg & !names(each_vd_args)[[each_arg]] %in% arg_names) {
                msg(glue("Validation function ",
                         "`{names(validate_list)[each_vd]}`",
                         " requires arg `{names(each_vd_args)[[each_arg]]}` ",
                         "with no defaults but it is not in your args list"),
                    "error")
            }
        }, list(1))
        arg_names  %in% names(each_vd_args)
    }, list(1))
    for(index in seq_along(validate_list)){
        result <- shinyCatch(do.call(
            validate_list[[index]], args = args[inject_args[[index]]]
        ))
        if (is.null(result)) {
            shinyCatch(stop(glue("Error(s) while running through validator: ",
                                 "{names(validate_list)[index]} ")),
                       blocking_level = "error")
        }
        if (!rlang::is_bool(result) | length(result) > 1) {
            msg(glue("Function {names(validate_list)[index]}",
                     "should only return `SINGLE` TRUE or FALSE"),
                "error")}
        if (is.null(names(result))) {
            msg(glue("Function {names(validate_list)[index]}",
                     "return needs to be named"), "error")}
        if (!result) {
            failed_msg <- result %>% names %>%
                c("Validation Failed: ", .) %>%
                glue_collapse(sep = "\n- ")
            shinyCatch(stop(failed_msg), blocking_level = "error")
        }
    }
    shinytoastr::toastr_success(glue("{title} Passed"),
                                position = "bottom-right",
                                timeOut = 3500)
    return(invisible())
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
