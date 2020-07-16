################## A Collections of server utilities############################

## use on top of shiny

#' @import shiny shinytoastr stringr magrittr glue htmltools shinyWidgets
NULL


#' Catch  error, warning, message text catcher
#' @description Catch error, warning, message by a toastr bar on shiny front end
#' also log the text on backend console. Will return original value if not
#' blocking at "warning" "message" level, and return `NULL` at error level.
#' If blocks at `error`, function will be stopped and other code in the same
#' reactive context will be blocked. If blocks at `warning` level, warning and
#' error will be blocked; `message` level blocks will 3 levels.
#' @param expr expression
#' @param position toastr position: c("top-right", "top-center", "top-left",
# "top-full-width", "bottom-right", "bottom-center", "bottom-left",
# "bottom-full-width")
#' @param blocking If encounter any error, warning or message, continue or
#' stop
#' @param blocking_level if `non_blocking` is FALSE, at what level you want to
#' block the execution, one of "error", "warning", "message"
#'
#' @return see description
#'
#' @export
#'
#' @example
#' ui <- fluidPage(
#'     useToastr(),
#'     actionButton("btn1","error and blocking"),
#'     actionButton("btn2","error no blocking"),
#'     actionButton("btn3","warning but still returns value"),
#'     actionButton("btn4","warning but blocking returns"),
#'     actionButton("btn5","message"),
#'
#'     textOutput("text")
#' )
#' server <- function(input, output, session) {
#'     fn_warning <- function() {
#'          warning("this is a warning!")
#'          return(1)
#'          }
#'     observeEvent(input$btn1, {
#'         shinyCatch(stop("error with blocking"), blocking_level = "error")
#'          print("You shouldn't see me")
#'     })
#'     observeEvent(input$btn2, {
#'         shinyCatch(stop("error without blocking"))
#'         print("I am not blocked by error")
#'     })
#'     observeEvent(input$btn3, {
#'         return_value <- shinyCatch(fn_warning())
#'         print(return_value)
#'     })
#'     observeEvent(input$btn4, {
#'         return_value <- shinyCatch(fn_warning(), blocking_level = "warning")
#'         print(return_value)
#'         print("other things")
#'     })
#'     observeEvent(input$btn5, {
#'         shinyCatch(message("message"))
#'     })
#' }
#' shinyApp(ui, server)
shinyCatch <- function(expr, position = "bottom-right", blocking_level = "none") {
    toastr_actions <- list(
        message = function(m) {
            msg(m$message, "SPS-INFO", "blue")
            toastr_info(message = remove_ANSI(m$message),
                        position = position, closeButton = TRUE,
                        timeOut = 3000, preventDuplicates = TRUE)
        },
        warning = function(m) {
            msg(m$message, "SPS-WARNING", "orange")
            toastr_warning(message = remove_ANSI(m$message),
                           position = position, closeButton = TRUE,
                           timeOut = 5000, preventDuplicates = TRUE)
        },
        error = function(m) {
            msg(m$message, "SPS-ERROR", "red")
            toastr_error(
                message = remove_ANSI(m$message), position = position,
                closeButton = TRUE, timeOut = 0, preventDuplicates = TRUE,
                title = "There is an error", hideDuration = 300
            )
        }
    )
    switch(tolower(blocking_level),
           "error" = tryCatch(
               suppressMessages(suppressWarnings(withCallingHandlers(
                   expr,
                   message = function(m) toastr_actions$message(m),
                   warning = function(m) toastr_actions$warning(m)
               ))),
               error = function(m) {
                   toastr_actions$error(m)
                   shiny:::reactiveStop(class = "validation")
               }),
           "warning" = tryCatch(
               suppressMessages(withCallingHandlers(
                   expr,
                   message = function(m) toastr_actions$message(m)
               )),
               warning = function(m) {
                   toastr_actions$warning(m)
                   shiny:::reactiveStop(class = "validation")
                   },
               error = function(m) {
                   if(!is.empty(m$message)) toastr_actions$error(m)
                   shiny:::reactiveStop(class = "validation")
               }),
           "message" = tryCatch(
               expr,
               message = function(m) {
                   message = toastr_actions$message(m)
                   shiny:::reactiveStop(class = "validation")
                   },
               warning = function(m) {
                   toastr_actions$warning(m)
                   shiny:::reactiveStop(class = "validation")
                   },
               error = function(m) {
                   if(!is.empty(m$message)) toastr_actions$error(m)
                   shiny:::reactiveStop(class = "validation")
               }),
           tryCatch(
               suppressMessages(suppressWarnings(withCallingHandlers(
                   expr,
                   message = function(m) toastr_actions$message(m),
                   warning = function(m) toastr_actions$warning(m)
               ))),
               error = function(m) {
                   toastr_actions$error(m)
                   return(NULL)
               }
           )
    )
}


#' check name space in server
#' check name space and pop up warnings in shiny if package is missing
#'
#' @param session shiny session
#' @param cran_pkg vector of strings
#' @param bioc_pkg vector of strings
#' @param github vector of strings, github package must specify user name, c("user1/pkg1", "user2/pkg2")
#' @param quietly bool, should progress and error messages be suppressed?
#'
#' @return sweet alert massage
#' @export
#'
#' @ example
#' shinyApp(ui = shinyUI(
#'     fluidPage(actionButton("haha", "haha"))
#' ), server = function(input, output, session) {
#'     observeEvent(input$haha, shinyCheckSpace(session, cran_pkg = "1",
#'                  bioc_pkg = "haha", github = "sdasdd/asdsad"))
#' })
shinyCheckSpace <- function(session, cran_pkg = NULL, bioc_pkg = NULL, github = NULL, quietly = FALSE) {
    missing_cran <- checkNameSpace(cran_pkg, quietly, from = "CRAN")
    missing_bioc <- checkNameSpace(bioc_pkg, quietly, from = "BioC")
    github_pkg <- github %>% str_remove("^.*/")
    missing_github_pkg <- checkNameSpace(github_pkg, quietly, from = "GitHub")
    missing_github <- github[github_pkg %in% missing_github_pkg]
    cran_cmd <- if (is.empty(missing_cran)) "" else
        paste0("install.packages(c('", paste0(missing_cran, collapse = "', '"), "'))")
    bioc_cmd <- if (is.empty(missing_bioc)) "" else
        paste0(
        'if (!requireNamespace("BiocManager", quietly=TRUE))
        install.packages("BiocManager")\n',
        "BiocManager::install(c('", paste0(missing_bioc, collapse = "', '"), "'))"
        )
    github_cmd <- if (is.empty(missing_github)) "" else
        paste0(
            'if (!requireNamespace("BiocManager", quietly=TRUE))
                install.packages("BiocManager")\n',
            "BiocManager::install(c('", paste0(missing_github, collapse = "', '"), "'))"
        )

    if (length(missing_cran) + length(missing_bioc) + length(missing_github) > 0) {
        shinyWidgets::sendSweetAlert(
            session = session,
            title = "Please install required packages manually",
            text = tags$div(style = "
                        background-color: #FA5858;
                        text-align: left;
                        overflow: auto;
                        white-space: pre;
                        color: black;

                        ",
                    p(cran_cmd),
                    p(bioc_cmd),
                    p(github_cmd)
                    ),
            html = TRUE,
            type = "error"
        )
        return(FALSE)
    } else {
        shinytoastr::toastr_success(
            message = "You have all required packages for this tab",
            position = "bottom-right")
        return(TRUE)
    }
}

#' Server side function for dynamicFile
#' @param input shiny server input
#' @param session shiny server session
#' @param id input file element ID.
#' Do not us `ns()` to wrap the id on server side if inside module
#'
#' @return reactive dataframe, need to extract the value inside reactive
#' expression, observe, or inside `isolate`
#' @export
#'
#' @examples
#' library(shiny)
#' library(shinyFiles)
#' library(shinyjs)
#' options(sps = list(mode='server'))
#' ui <- fluidPage(
#' useShinyjs(),
#' dynamicFile("getFile"),
#' textOutput("txt_file")
#' )
#'
#' server <- function(input,output,session){
#'     runjs('$(".sps-file input").attr("readonly", true)')
#'     myfile <- dynamicFileServer(input,session, id = "getFile")
#'     observe({
#'         print(myfile()) # remember to use `()` for reactive value
#'     })
#' }
#' shinyApp(ui = ui, server = server)
dynamicFileServer <- function(input,session, id){
    file_return <- reactiveVal(NULL)
    if (getOption("sps")$mode == "local") {
        roots <- c(current=getwd(), getVolumes()())
        shinyFileChoose(input, id, roots = roots, session = session)
        observeEvent(input[[id]],
            file_return({
                req(is.list(input[[id]]))
                file_selected <- parseFilePaths(roots, input[[id]])
                updateTextInput(inputId = glue("{id}-text"),
                                session = session,
                                placeholder = unname(file_selected$datapath))
                as.data.frame(file_selected)
            })
        )
        file_return
    } else {
        observe(file_return(input[[id]]))
        file_return
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

#' A draggable progress panel
#' Use `pgPaneUI` on UI side and use `pgPaneUpdate` to update it. The UI only
#' renders correctly inside `shinydashboard` or `shinydashboardPlus`.
#' @param pane_id Progress panel main ID, use `ns` wrap it on `pgPaneUI` but not
#' on `pgPaneUpdate` if using shiny module
#' @param pg_id a character string of ID for the progress you want to update.
#'  Do not use \code{ns(pg_id)} to wrap it on server
#' @param value 0-100 real number to update the progress you use `pg_id` to
#' choose
#' @param session current shiny session
#' @example
#' library(shiny)
#' library(shinydashboard)
#' library(shinytoastr)
#' ui <- dashboardPage(header = dashboardHeader(),
#'                     sidebar = dashboardSidebar(),
#'                     body = dashboardBody(
#'                         useSps(),
#'                         actionButton("a", "a"),
#'                         actionButton("b", "b"),
#'                         sliderInput("c", min = -100, max = 100, value = 0,
#'                                     label = "c"),
#'                         pgPaneUI("thispg", c("this a", "this b", " this c"),
#'                                  c("a", "b", "c"), "Example Progress")
#'                     )
#' )
#' server <- function(input, output, session) {
#'     observeEvent(input$a, {
#'         for(i in 1:10){
#'             pgPaneUpdate("thispg", "a", i*10)
#'             Sys.sleep(0.3)
#'         }
#'     })
#'     observeEvent(input$b, {
#'         for(i in 1:10){
#'             pgPaneUpdate("thispg", "b", i*10)
#'             Sys.sleep(0.3)
#'         }
#'     })
#'     observeEvent(input$c, pgPaneUpdate("thispg", "c", input$c))
#' }
#' shinyApp(ui, server)
pgPaneUpdate <- function(pane_id, pg_id, value,
                         session = getDefaultReactiveDomain()){
    shinyCatch({
        assert_that(is.character(pane_id))
        assert_that(is.character(pg_id))
        assert_that(value >= 0 & value <= 100,
                    msg = "Progress value needs to be 0-100")
        updateProgressBar(session, id = glue("{pg_id}-pg"), value = value)
        if(inherits(session, "session_proxy")){
            pane_id <- session$ns(pane_id)
            pg_id <- session$ns(pg_id)
        }
        session$sendCustomMessage(
            type = "sps-update-pg",
            message = list(
                panel_id = pane_id,
                which_pg = pg_id,
                value = value
        ))
    }, blocking_level = "error")

}

#' Add and get data between shiny modules
#'
#' These function groups are designed to be used inside shiny modules
#' @param data any type of R object you want to store and use in other tabs
#' @param shared the SPS shared reactivevalues object
#' @param type one of data, plot
#' @param tab_id tab ID of current tab if using `addData` method and tab
#' ID to get data from if using `getData`.
#' @return Nothing to return with `add` method and returns original object for
#' the `get` method
#' @export
#' @examples
#' library(shiny)
#' library(shinytoastr)
#' resolveOptions()
#' ui <- fluidPage(
#'     useToastr(),
#'     actionButton("add", "add"),
#'     actionButton("get", "get"),
#'     actionButton("wrong", "when it gets wrong")
#' )
#' server <- function(input, output, session) {
#'     tab_info <- tibble::tibble(
#'         Tab_name = 'df_count',
#'         Display_label = 'Count Table',
#'         type = 'data',
#'         image = ''
#'     )
#'     shared <- reactiveValues()
#'     data <- tibble::tibble(this = 123)
#'     cat('before adding\n')
#'     print(shared)
#'     observeEvent(input$add, {
#'         addData(data, shared, "thistab")
#'         cat('after adding\n')
#'         print(shared) # watch the data_intask object is created
#'     })
#'     observeEvent(input$get, {
#'         cat("get data\n")
#'         getData('thistab', shared)
#'     })
#'     observeEvent(input$wrong, {
#'         cat("get wrong data\n")
#'         getData('not_there', shared)
#'     })
#' }
#' shinyApp(ui, server)
addData <- function(data, shared, tab_id) {
    shinyCatch({
        assert_that(inherits(shared, "reactivevalues"))
        assert_that(is.character(tab_id))
        findTabInfo(tab_id)
        if(not_empty(shared[['data']][[tab_id]]) & getOption('sps')$verbose)
            message(c(glue("found {tab_id} has already been added to "),
                           "`shared$data_intask` list, overwrite"))
        shared[['data']][[tab_id]] <- data
        if(getOption('sps')$verbose) {
            info <- glue("Data for namespace {tab_id} added")
            message(info)
            toastr_info(info, timeOut = 3000, position = "bottom-right")
        }
    }, blocking_level = "error")
}


#' @rdname addData
#' @export
getData <- function(tab_id, shared){
    shinyCatch({
        assert_that(inherits(shared, "reactivevalues"))
        assert_that(is.character(tab_id) & length(tab_id) < 2,
                    msg = "A character string of one tab name each time")
        tab_info <- findTabInfo(tab_id)$tab_labels
        if(is.empty(shared[['data']][[tab_id]]))
        stop(glue("Data from tab `{tab_info}` is empty"))
        if(getOption('sps')$verbose){
            success_info <- glue("data for tab `{tab_info} found`")
            toastr_info(success_info, timeOut = 3000, position = "bottom-right")
        }
        return(shared[['data']][[tab_id]])
    }, blocking_level = "error")
}


spsWarnings <- function(session){
    sps_warnings <- list()
    if(getOption("sps")$dev) {
        msg("Developer mode is on. you shouldn't deploy app with this mode",
            "SPS-DANGER", "red")
        sps_warnings[['dev']] <- h4(glue("You are on developer mode"))
    }
    sendSweetAlert(session = session, html = TRUE,
                   '<p style="color:var(--danger)">DANGER</p>',
                   div(class = "sps-warning",
                       tagList(sps_warnings)
                       ),
                   type = "error"
    )
}



