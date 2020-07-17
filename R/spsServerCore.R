# SPS core server functions, can only be used under SPS framework


#' Workflow Progress tracker server logic
#' @description use it on the top level server not inside a module. Only
#' designed for workflow tabs. For other purpose, use `pgPaneUI`.
#' @param shared the shared object
#'
#' @return reactive renderUI object
#' @export
#' @seealso pgPaneUpdate
#' @examples
#' wfProgressPanel(shared)
wfProgressPanel <- function(shared){
    renderUI({
        total_progress <- sum(as.numeric(shared$wf_flags))/0.03
        timelineBlock(reversed = FALSE,
                      timelineItem(
                          title = "Targets file",
                          icon = timeline_icon(shared$wf_flags$targets_ready),
                          color = timeline_color(shared$wf_flags$targets_ready),
                          border = FALSE,
                          progressBar(
                              "pg_target",  striped = TRUE, status = "primary",
                              timeline_pg(shared$wf_flags$targets_ready))
                      ),
                      timelineItem(
                          title = "Workflow Rmd",
                          icon = timeline_icon(shared$wf_flags$wf_ready),
                          color = timeline_color(shared$wf_flags$wf_ready),
                          border = FALSE,
                          progressBar(
                              "pg_rmd", striped = TRUE, status = "primary",
                              timeline_pg(shared$wf_flags$wf_ready))
                      ),
                      timelineItem(
                          title = "Config",
                          icon = timeline_icon(shared$wf_flags$wf_conf_ready),
                          color = timeline_color(shared$wf_flags$wf_conf_ready),
                          border = FALSE,
                          progressBar(
                              "pg_config", striped = TRUE, status = "primary",
                              timeline_pg(shared$wf_flags$wf_conf_ready))
                      ),
                      timelineLabel("Ready",
                                    color = if(all(as.logical(shared$wf_flags)))
                                        "olive"
                                    else "orange"),
                      div(style = "margin-left: 60px; margin-right: 15px;",
                          progressBar(
                              "pg_wf_all", total_progress, striped = TRUE,
                              status = timline_pg_status(total_progress)
                          )
                      )
        )
    })
}


#' Warning toast under some options when app starts
#'
#' @param session
#'
#' @return
#' @export
#'
#' @examples
spsWarnings <- function(session){
    sps_warnings <- list()
    if(getOption("sps")$dev) {
        msg("Developer mode is on. you shouldn't deploy app with this mode",
            "SPS-DANGER", "red")
        sps_warnings[['dev']] <- h4("You are on developer mode")
    }
    if(getQueryString() == "admin"){
        msg("You admin page url is default, consider change it to a different one",
            "SPS-DANGER", "red")
        sps_warnings[['admin']] <- h4("Change default admin page url")
    }

    if(getOption("sps")$warning_toast){
        sendSweetAlert(session = session, html = TRUE,
                       '<p style="color:var(--danger)">DANGER</p>',
                       div(class = "sps-warning",
                           tagList(sps_warnings)
                       ),
                       type = "error"
        )
    }
}


#' Load tibbles to server
#' @description load a file to server end. Designed to be used with the input
#' file source switch button. Use `vroom` to load the file
#' @param choice where this file comes from, from 'upload' or example 'eg'?
#' @param df_init a tibble to return if `upload_path` or `eg_path` is not
#' provided. Return a 8x8 empty tibble if not provided
#' @param upload_path when `choice` is "upload", where to load the file, will
#' return `df_init` if this param is not provided
#' @param eg_path when `choice` is "eg", where to load the file, will
#' return `df_init` if this param is not provided
#' @param comment comment characters when load the file, see help file of `vroom`
#' @param delim delimiter characters when load the file, see help file of `vroom`
#' @param col_types columns specifications, see help file of `vroom`
#' @param ... other params for vroom, see help file of `vroom`
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(shiny)
#' library(shinyWidgets)
#' ui <- fluidPage(
#'     shinyWidgets::radioGroupButtons(
#'         inputId = "data_source", label = "Choose your data file source:",
#'         selected = "upload",
#'         choiceNames = c("Upload", "Example"),
#'         choiceValues = c("upload", "eg")
#'     ),
#'     fileInput("df_path", label = "input file"),
#'     dataTableOutput("df")
#' )
#'
#' server <- function(input, output, session) {
#'     tmp_file <- tempfile(fileext = ".csv")
#'     write.csv(iris, file = tmp_file)
#'     data_df <- reactive({
#'         loadDF(choice = input$data_source, upload_path = input$df_path$datapath,
#'                delim = ",", eg_path = tmp_file)
#'     })
#'     output$df <- renderDataTable(data_df())
#' }
#' shinyApp(ui, server)
loadDF <- function(choice, df_init=NULL, upload_path=NULL, eg_path=NULL,
                   comment = "#", delim = "\t",
                   col_types = cols(), ...){
    df <- shinyCatch({
        choice <- match.arg(choice, c("upload", "eg"))
        df_init <-
            if(is.empty(df_init)) {
                data.frame(matrix("", 8,8), stringsAsFactors = FALSE) %>%
                    tibble::as_tibble()
            }
        else {df_init}
        if(!any(class(df_init) %in% c("tbl_df", "tbl", "data.frame"))) {
            stop("df_init need to be dataframe or tibble")
        }
        df_init <- tibble::as_tibble(df_init)
        if (choice == "upload" & is.empty(upload_path)) return(df_init)
        if (choice == "eg" & is.empty(eg_path)) return(df_init)
        upload_path <- switch(choice,
                              "upload" = upload_path,
                              "eg" = eg_path,
        )
        df <- shinyCatch(vroom(
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
#'
#' @return if any validation function returns FALSE, a error pop-up will show
#' in the shiny app.
#' @export
#'
#' @examples
#' library(shiny)
#' library(shinytoastr)
#' df_validate_common <- list(
#'     vd1 = function(df, apple){
#'         print(apple)
#'         if (is(df, "data.frame")) {result <- c(" " = TRUE)}
#'         else {result <- c("Input is not dataframe or tibble" = FALSE)}
#'         return(result)
#'     },
#'     vd2 = function(df, banana, orange="orange"){
#'         print(banana)
#'         print(orange)
#'         # change < to > in next line to see failure
#'         if (nrow(df) < 10) {result <- c(" " = TRUE)}
#'         else {result <- c("Need more than 10 rows" = FALSE)}
#'         Sys.sleep(1)
#'         return(result)
#'     }
#' )
#' ui <- fluidPage(
#'     actionButton("btn", "this btn"),
#'     useToastr(),
#'     use_waitress()
#' )
#' server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'         spsValidator(df_validate_common,
#'                      args = list(df = data.frame(),
#'                                  apple = "apple",
#'                                  banana = "banana"))
#'         print(123)
#'     })
#' }
#' shinyApp(ui, server)
spsValidator <- function(validate_list, args = list(), title = "Validation"){
    # pre checks
    if(!is.list(args)) msg("Args must be in a list", "error")
    if(!is.list(validate_list)) msg("Validate_list must be in a list", "error")
    if(any(is.null(validate_list))) msg("All functions need to be named", "error")
    arg_names <- names(args)
    if(any(is.null(arg_names))) msg("All args must be named", "error")
    # check if all required args are provided
    inject_args <- sapply(seq_along(validate_list), function(each_vd) {
        if(!is.function(validate_list[[each_vd]])) {
            msg("Each item in `validate_list` must be a function", "error")}
        each_vd_args <- formals(validate_list[[each_vd]])
        sapply(seq_along(each_vd_args), function(each_arg) {
            required_arg <- rlang::is_missing(each_vd_args[[each_arg]])
            if(required_arg & !names(each_vd_args)[[each_arg]] %in% arg_names) {
                msg(glue("Validation function ",
                         "`{names(validate_list)[each_vd]}`",
                         " requires arg `{names(each_vd_args)[[each_arg]]}` ",
                         "with no defaults but it is not in your args list"),
                    "error")
            }
        })
        arg_names  %in% names(each_vd_args)
    }, simplify = FALSE)
    for(index in seq_along(validate_list)){
        result <- shinyCatch(do.call(
            validate_list[[index]], args = args[inject_args[[index]]]
        ))
        if (is.null(result)) {
            shinyCatch(stop(glue("Error(s) while running through validator: ",
                                 "{names(validate_list)[index]} ")),
                       blocking = "error")
        }
        if (!is_bool(result) | length(result) > 1) {
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
            shinyCatch(stop(failed_msg), blocking = "error")
        }
    }
    toastr_success(glue("{title} Passed"), position = "bottom-right",
                   timeOut = 3500)
    return(invisible())
}
