#' SPS snapshots container
#'
#' Initiate this container at the global level. This container is used to
#' communicate plotting tabs with the canvas tab
#' @importFrom R6 R6class
#'
#' @examples
#' library(shiny)
#' library(shinydashboard)
#' library(shinytoastr)
#' library(shinyjs)
#' plots = plotContainer$new()
#'
#' mod1_UI <- function(id) {
#'     ns <- NS(id)
#'     tagList(
#'         actionButton(ns("render"), "render"),
#'         jqui_resizable(plots$addUI(plotlyOutput(ns("plot1")), id)),
#'         sliderInput(ns("slide"), label = "rows", min = 1, max = nrow(iris), value = nrow(iris))
#'
#'     )
#' }
#' mod1 <- function(input, output, session, shared) {
#'     observeEvent(input$render, {
#'         output$plot1 <- plots$addServer(renderPlotly, 'mod1', {
#'             ggplotly(ggplot(iris[1:input$slide, ], aes(Sepal.Length, Sepal.Width)) +
#'                          geom_point(aes(colour = Species)))
#'         })
#'         shared$snap_signal <- plots$notifySnap("mod1")
#'         req(shared$snap_signal)
#'         toastr_info(glue("Snapshot {glue_collapse(shared$snap_signal, '-')} added to canvas"),
#'                     position = "bottom-right")
#'     })
#' }
#' mod2_UI <- function(id){
#'     ns <- NS(id)
#'     tagList(
#'         actionButton(ns("refresh"), 'refresh'),
#'         sliderTextInput(
#'             inputId = ns("ncols"),
#'             label = "Number of columns per row to initiate canvas:",
#'             choices = c(1:4, 12), selected = 2, grid = TRUE
#'         ),
#'         fluidRow(uiOutput(ns("new")), class = "sps-canvas")
#'     )
#' }
#'
#' mod2 <- function(input, output, session, shared) {
#'     ns <- session$ns
#'     make_plots <- reactiveValues(ui = list(), server = list())
#'     observeEvent(shared$snap_signal, {
#'         tab_id <- shared$snap_signal[1]
#'         new_plot_id <- glue("{tab_id}-{shared$snap_signal[2]}")
#'         print(new_plot_id)
#'         make_plots$ui[[new_plot_id]] <- list(plots$getUI(tab_id,
#'                                                          ns(new_plot_id)),
#'                                              ns(new_plot_id))
#'         make_plots$server[[new_plot_id]] <- plots$getServer(tab_id)
#'     })
#'     observeEvent(input$refresh, {
#'         ui <- make_plots$ui
#'         output$new <- renderUI({
#'             sapply(seq_along(ui), function(i){
#'                 column(
#'                     width = 12/isolate(input$ncols),
#'                     class = "collapse in",
#'                     id = glue("{ui[[i]][2]}-container"),
#'                     div(class = "snap-drag bg-primary",
#'                         h4(glue("Plot {ui[[i]][2]}")),
#'                         tags$button(
#'                             class = "btn action-button canvas-close",
#'                             icon("times"),
#'                             `data-toggle`="collapse",
#'                             `data-target`=glue("#{ui[[i]][2]}-container"),
#'                             `plot-toggle` = ui[[i]][2]
#'                         )
#'                     ),
#'                     jqui_resizable(make_plots$ui[[i]][[1]]),
#'                     tags$script(glue(.open = '@', .close = '@',
#'                                      '$("#@ui[[i]][2]@-container")',
#'                                      '.draggable({ handle: ".snap-drag"})'))
#'                 )
#'             }, simplify = FALSE) %>%{
#'                 fluidRow(id = ns('plots'),
#'                          tags$head(tags$style(
#'                              '
#'                              .snap-drag {
#'                              opacity: 0;
#'                              };'),
#'                              tags$style(
#'                                  '
#'                              .snap-drag button{
#'                              position: absolute;
#'                              outline: none;
#'                              background-color: Transparent;
#'                              top: 2px;
#'                              right: 4px;
#'                              };'),
#'                              tags$style(
#'                                  '
#'                              .snap-drag h4{
#'                              margin-bottom: 0;
#'                              padding-bottom: 10px;
#'                              };'),
#'                              tags$style(
#'                                  '
#'                              .snap-drag button:focus {
#'                              outline: 0 !important;
#'                              box-shadow: none !important;
#'                              };'),
#'                              tags$style('.snap-drag:hover {
#'                              opacity: 1;
#'                              }
#'                              '
#'                              )),
#'                          tagList(.)
#'                 )
#'             }
#'         })
#'         for(plot_id in names(make_plots$server)){
#'             output[[plot_id]] <- make_plots$server[[plot_id]]
#'         }
#'     })
#'     observeEvent(input$hide_title, {
#'         shinyjs::toggleClass(selector = ".snap-drag", class = "collapse")
#'     })
#'
#' }
#' ui <- dashboardPagePlus(
#'     header = dashboardHeaderPlus(),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'         useShinyjs(),
#'         useToastr(),
#'         mod1_UI("mod1"),
#'         mod2_UI("mod2")),
#'
#'     rightsidebar = rightSidebar(),
#'     title = "Test",
#' )
#' server <- function(input, output, session) {
#'     shared = reactiveValues()
#'     callModule(mod1, "mod1",  shared)
#'     callModule(mod2, "mod2",  shared)
#' }
#' shinyApp(ui, server)
plotContainer <- R6::R6Class("plot_container",
    public = list(
        initialize = function(){
            msg("Created a plot container R6 object")
            private$verbose <- private$verbosity()
            msg(c("Current verbosity for plot contianer is ", private$verbose))
        },

        #' @field a list of plot UI snapshots will be stored here. You shouldn't
        #' manually edit this list, use `add/getUI` method
        plot_ui = list(),

        #' @field a list of plot server snapshots will be stored here. You shouldn't
        #' manually edit this list, use `add/getServer` method
        plot_server = list(),

        #' @description
        #' add plot UI to the container. use it to wrap around the original plot
        #'  out put function
        #' @param plot_DOM Plotting output function, like `plotOutput`
        #' @param tab_id unique ID, usually use in a module and use the tab ID
        addUI = function(plot_DOM, tab_id){
            if(!inherits(plot_DOM, c("shiny.tag", "shiny.tag.list")))
                msg("plot UI is not shinytag or shiny.taglist", "error")
            DOM_id <- private$tagAttrib(plot_DOM, 'id')
            if (length(DOM_id) < 1)
                msg(c("Can't find ID for this plot UI output function. If you ",
                      "are sure you have use the right function, then this ",
                      "plotting UI function is an exception.\n",
                      "Please open an issue on our Github page.",
                      "https://github.com/systemPipeR/systemPipeShiny"), "warning")
            if(not_empty(self$plot_ui[[tab_id]]) & private$verbose)
                msg(glue("Plot UI for this tab `{tab_id}` already exists ",
                         "in the container, overwrite"))
            self$plot_ui[[tab_id]] <- plot_DOM
            return(plot_DOM)
        },

        #' @description Get plot UI to the container.
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @param plot_id_new string, usually if taking a snapshot of a plot DOM,
        #' they can't use the same ID on HTML. When get the UI, change the ID
        #' to a new value
        #' @return a saved plotting DOM
        getUI = function(tab_id, plot_id_new = NULL){
            ui <- self$plot_ui[[tab_id]]
            if(is.null(ui)) msg(glue("Can't find `{id}` in plot_ui list"), "error")
            if(!is.null(plot_id_new)){
                ui <- private$tagAttrib(ui, 'id', plot_id_new)
            }
            return(ui)
        },


        #' @description
        #' add plot server function to the container. use it to wrap around the
        #' original plot output server function, like `renderPlot`, `renderPlotly`
        #' @param render_func plot output server function
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @param expr the reactive expression to render the plot, same as `expr`
        #' in other render function
        #' @param env default `parent.frame()`, see shiny documents
        #' @param quoted Is you `expr` quoted?
        #' @param ... additional args to pass to the original render function
        addServer = function(render_func, tab_id, expr,
                             env = parent.frame(), quoted = FALSE, ...){
            expr_func <- exprToFunction(expr, env, quoted)
            if(!inherits(render_func, "function"))
                stop("plot server is not a tab_id")
            if(not_empty(self$plot_server[[tab_id]]) & private$verbose){
                msg(glue("Plot server with id {tab_id} already exists ",
                         "in the container, overwrite"))}
            self$plot_server[[tab_id]] <- list(func = render_func,
                                               args = list(isolate(expr_func()), ...))
            return(render_func(expr_func(), ...))
        },


        #' @description Get plot UI to the container.
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @return saved plot server function
        getServer = function(tab_id){
            render_expr <- self$plot_server[[tab_id]]
            if(is.null(render_expr))
                msg(glue("Can't find `{tab_id}` in plot_server list"), "error")
            return(base::do.call(render_expr$func, render_expr$args))
        },


        #' @description notify the snapshot tab a snapshot has been added
        #'
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @param skip positive integer, This function is usually bound to the
        #' plotting button. When clicked, the first number of `skip` clicks will
        #' not add the snapshot to the canvas tab.
        #' @param reset bool, to reset the count or not
        #' @return a vector of `tab_id` and count number as a character string
        notifySnap = function(tab_id, skip = 1, reset = FALSE){
            if(reset) {
                private$notify_list[[tab_id]] <- NULL
                return(invisible())
                }
            assert_that(is.number(skip))
            notify_index <- private$notify_list[[tab_id]]
            if(is.null(notify_index)) {
                private$notify_list[[tab_id]] <- 1}
            else {private$notify_list[[tab_id]] <- notify_index + 1}
            notify_index <- private$notify_list[[tab_id]]
            if (notify_index <= skip) return(NULL)
            else return(c(tab_id, notify_index - skip))
        }
    ),
    private = list(
        tagAttrib = function(taglist, attrib, changeto = NULL){
            is_tag <- FALSE
            attr_value <- NULL
            if(inherits(taglist, "shiny.tag"))
                {taglist <- tagList(taglist); is_tag <- TRUE}
            for (i in seq_along(taglist)){
                if (!inherits((taglist[[i]]), "shiny.tag")) next
                attr_value = taglist[[i]]$attribs[[attrib]]
                if(!is.null(attr_value)){
                    if (is.null(changeto)){return(attr_value)}
                    else {
                        taglist[[i]]$attribs[[attrib]] <- as.character(changeto)
                        if(is_tag) return(taglist[[1]])
                        else return(taglist)
                    }
                }
            }
            if(is.null(attr_value))
                msg(glue("Can't find attribute `{attrib}`"), "warning")
        },
        verbosity = function(){
            verbose <- getOption('sps')$verbose
            if(is.null(verbose)) verbose <- FALSE
            return(verbose)
        },
        verbose = FALSE,
        notify_list = list()
    )
)
