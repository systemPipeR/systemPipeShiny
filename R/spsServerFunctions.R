################## A Collections of server utilities############################

## use on top of shiny

#' @import shiny shinytoastr stringr magrittr glue htmltools shinyWidgets
NULL


#' Catch  error, warning, message text catcher
#' @description Catch error, warning, message by a toastr bar on shiny front end
#' also log the text on backend console
#' @param expr expression
#' @param position toastr position: c("top-right", "top-center", "top-left",
# "top-full-width", "bottom-right", "bottom-center", "bottom-left",
# "bottom-full-width")
#' @param non_blocking If encounter any error, warning or message, continue or
#' stop
#'
#' @return if `non-blocking`,  warning or message return original value, error
#' return `NULL`. If `blocking`, any error, warning, message will return NULL,
#' otherwise original value
#' @export
#'
#' @example
#' ui <- fluidPage(
#'     useToastr(),
#'     actionButton("btn1","Click me1"),
#'     actionButton("btn2","Click me2"),
#'     actionButton("btn3","Click me3"),
#'
#'     textOutput("text")
#' )
#' server <- function(input, output, session) {
#'     observeEvent(input$btn1, {
#'         shinyCatch(stop("stop"))
#'     })
#'     observeEvent(input$btn2, {
#'         shinyCatch(warning("warning"))
#'     })
#'     observeEvent(input$btn3, {
#'         shinyCatch(message("message"))
#'     })
#' }
#' shinyApp(ui, server)
shinyCatch <- function(expr, position = "bottom-right", non_blocking = TRUE) {
    if (non_blocking) {
        tryCatch(
            withCallingHandlers(
                expr,
                warning = function(w) {
                    toastr_warning(message = remove_ANSI(w$message), position = position,
                                   closeButton = TRUE, timeOut = 5000)
                },
                message = function(m) {
                    toastr_info(message = remove_ANSI(m$message), position = position,
                                closeButton = TRUE, timeOut = 3000)
                }
            ),
            error = function(e) {
                msg(e$message, "SPS-ERROR", "red")
                toastr_error(
                    message = remove_ANSI(e$message), position = position,
                    closeButton = TRUE, timeOut = 0,
                    title = "There is an error", hideDuration = 300,
                )
                return(NULL)
            }
        )
    } else {
        tryCatch(
            expr,
            error = function(e) {
                msg(e$message, "SPS-ERROR", "red")
                toastr_error(
                    message = remove_ANSI(e$message), position = position,
                    closeButton = TRUE, timeOut = 0,
                    title = "There is an error", hideDuration = 300,
                )
                shiny:::reactiveStop(class = "validation")
            },
            warning = function(w) {
                msg(w$message, "SPS-WARNING", "orange")
                toastr_warning(message = remove_ANSI(w$message), position = position,
                               closeButton = TRUE, timeOut = 5000)
                shiny:::reactiveStop(class = "validation")
            },
            message = function(m) {
                msg(m$message, "SPS-INFO", "blue")
                toastr_info(message = remove_ANSI(m$message), position = position,
                            closeButton = TRUE, timeOut = 3000)
                shiny:::reactiveStop(class = "validation")
            })
    }
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
#'     observeEvent(input$haha, shinyCheckSpace(session, cran_pkg = "1", bioc_pkg = "haha", github = "sdasdd/asdsad"))
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
    })
    return(df)
}


#' Validate some inputs
#' @description Useful to check if a input dataframe, list or other things
#' meet the requirement. If any validation fails, it will show a pop-up message
#' and block any further code execution, similar to `shiny::req()`
#' @param validate_list a list of function which only returns a single value of
#' TRUE or FALSE. this value needs to be named. This name will be used as error
#' message
#' @param args a `named`(very important) list of arguments that will be used in
#' different function in  `validate_list`. `...` argument is not supported.
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
#'         if (nrow(df) > 10) {result <- c(" " = TRUE)}
#'         else {result <- c("Need more than 10 rows" = FALSE)}
#'         return(result)
#'     }
#' )
#'
#'
#' ui <- fluidPage(
#'     actionButton("btn", "this btn"),
#'     useToastr()
#' )
#'
#' server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'         spsValidator(df_validate_common,
#'                      args = list(df = data.frame(),
#'                                  apple = "apple",
#'                                  banana = "banana"))
#'         print(123)
#'     })
#' }
#'
#' shinyApp(ui, server)
spsValidator <- function(validate_list, args = list()){
    if(!is.list(args)) msg("Args must be in a list", "error")
    if(!is.list(validate_list)) msg("Validate_list must be in a list", "error")
    arg_names <- names(args)
    if(any(is.null(arg_names))) msg("All args must be named", "error")

    inject_args <- sapply(seq_along(validate_list), function(each_vd) {
        if(!is.function(validate_list[[each_vd]])) {
            msg("Each item in `validate_list` must be a function")}
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

    results <- sapply(seq_along(validate_list), function(index){
        result <- do.call(
            validate_list[[index]], args = args[inject_args[[index]]]
        )
        if (!is_bool(result) | length(result) > 1) {
            msg(glue("Function {names(validate_list)[index]}",
                     "should only return `SINGLE` TRUE or FALSE"),
                "error")}
        if (is.null(names(result))) {
            msg(glue("Function {names(validate_list)[index]}",
                     "return needs to be named"), "error")}
        result
    }, simplify = F)

    failed <- unlist(results) %>% {.[!.]} %>% names %>%
        c("Validation Failed: ", .) %>%
        glue_collapse(sep = "\n- ")
    shinyCatch(stop(failed), non_blocking = FALSE)
}
