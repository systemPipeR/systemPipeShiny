plotContainer <- R6::R6Class("plot_container",
    public = list(
        initialize = function(){
            msg("Created a plot container R6 object")
            private$verbose <- private$verbosity()
            msg(c("Current verbosity for plot contianer is ", private$verbose))
        },
        plot_ui = list(),
        plot_server = list(),
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
            # print(private$verbose)
            if(not_empty(self$plot_ui[[tab_id]]) & private$verbose)
                msg(glue("Plot UI for this tab `{tab_id}` already exists ",
                         "in the container, overwrite"))
            self$plot_ui[[tab_id]] <- plot_DOM
            return(plot_DOM)
        },
        getUI = function(tab_id, plot_id_new = NULL){
            ui <- self$plot_ui[[tab_id]]
            if(is.null(ui)) msg(glue("Can't find `{id}` in plot_ui list"), "error")
            if(!is.null(plot_id_new)){
                ui <- private$tagAttrib(ui, 'id', plot_id_new)
            }
            return(ui)
        },
        addServer = function(render_func, tab_id, expr,
                             env = parent.frame(), quoted = FALSE, ...){
            expr_func <- shiny::exprToFunction(expr, env, quoted)
            if(!inherits(render_func, "function"))
                stop("plot server is not a tab_id")
            if(not_empty(self$plot_server[[tab_id]]) & private$verbose){
                msg(glue("Plot server with id {tab_id} already exists ",
                         "in the container, overwrite"))}
            self$plot_server[[tab_id]] <- list(func = render_func,
                                               args = list(isolate(expr_func()), ...))
            return(render_func(expr_func(), ...))
        },
        getServer = function(tab_id){
            render_expr <- self$plot_server[[tab_id]]
            if(is.null(render_expr))
                msg(glue("Can't find `{tab_id}` in plot_server list"), "error")
            return(do.call(render_expr$func, render_expr$args))
        },
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

aaa = plotContainer$new()
mod1_UI <- function(id) {
  ns <- NS(id)
  tagList(
      actionButton(ns("render"), "render"),
      actionButton(ns("add"), "add"),
      jqui_resizable(aaa$addUI(plotlyOutput(ns("plot1")), id)),
      sliderInput(ns("slide"), label = "rows", min = 1, max = nrow(iris), value = nrow(iris))

  )
}

mod1 <- function(input, output, session, shared) {
    observeEvent(input$render, {
        output$plot1 <- aaa$addServer(renderPlotly, 'mod1', {
            ggplotly(ggplot(iris[1:input$slide, ], aes(Sepal.Length, Sepal.Width)) +
                geom_point(aes(colour = Species)))
        })
        shared$snap_signal <- aaa$notifySnap("mod1")
        req(shared$snap_signal)
        toastr_info(glue("Snapshot {glue_collapse(shared$snap_signal, '-')} added to canvas"),
                    position = "bottom-right")
    })
}
mod2_UI <- function(id){
    ns <- NS(id)
    tagList(
        actionButton(ns("refresh"), 'refresh'),
        sliderTextInput(
            inputId = ns("ncols"),
            label = "Number of columns per row to initiate canvas:",
            choices = c(1:4, 12), selected = 2, grid = TRUE
        ),
        actionButton(ns("hide_title"), "Hide Plot Title"),
        fluidRow(uiOutput(ns("new")), class = "sps-canvas")
    )
}

mod2 <- function(input, output, session, shared) {
    ns <- session$ns
    make_plots <- reactiveValues(ui = list(), server = list())
    observeEvent(shared$snap_signal, {
        tab_id <- shared$snap_signal[1]
        new_plot_id <- glue("{tab_id}-{shared$snap_signal[2]}")
        print(new_plot_id)
        make_plots$ui[[new_plot_id]] <- list(aaa$getUI(tab_id, ns(new_plot_id)), ns(new_plot_id))

        make_plots$server[[new_plot_id]] <- aaa$getServer(tab_id)
        # TODO add option of draggable
    })
    observeEvent(input$refresh, {
        ui <- make_plots$ui
        output$new <- renderUI({
            sapply(seq_along(ui), function(i){
                    column(
                        width = 12/isolate(input$ncols),
                        class = "collapse in",
                        id = glue("{ui[[i]][2]}-container"),
                        div(class = "snap-drag bg-primary",
                            h4(glue("Plot {ui[[i]][2]}")),
                            tags$button(
                                class = "btn action-button canvas-close", icon("times"),
                                `data-toggle`="collapse",
                                `data-target`=glue("#{ui[[i]][2]}-container"),
                                `plot-toggle` = ui[[i]][2]
                            )
                        ),
                        jqui_resizable(make_plots$ui[[i]][[1]]),
                        tags$script(glue(.open = '@', .close = '@',
                            '$("#@ui[[i]][2]@-container")',
                            '.draggable({ handle: ".snap-drag"})'))
                    )
            }, simplify = FALSE) %>%{
                fluidRow(id = ns('plots'),
                         tags$head(tags$style(
                             '
                             .snap-drag {
                             opacity: 0;
                             };'),
                             tags$style(
                                 '
                             .snap-drag button{
                             position: absolute;
                             outline: none;
                             background-color: Transparent;
                             top: 2px;
                             right: 4px;
                             };'),
                             tags$style(
                                 '
                             .snap-drag h4{
                             margin-bottom: 0;
                             padding-bottom: 10px;
                             };'),
                             tags$style(
                                 '
                             .snap-drag button:focus {
                             outline: 0 !important;
                             box-shadow: none !important;
                             };'),
                             tags$style('.snap-drag:hover {
                             opacity: 1;
                             }
                             '
                         )),
                         tagList(.)
                    )
            }
        })
        for(plot_id in names(make_plots$server)){
            output[[plot_id]] <- make_plots$server[[plot_id]]
        }
    })
    observeEvent(input$hide_title, {
       shinyjs::toggleClass(selector = ".snap-drag", class = "collapse")
    })

}
ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
        useShinyjs(),
        useToastr(),
  #       HTML('<script
  # src="http://code.jquery.com/ui/1.12.1/jquery-ui.min.js"
  # integrity="sha256-VazP97ZCwtekAsvgPBSUwPFKdrwD3unUfSGVYrahUqU="
  # crossorigin="anonymous"></script>'),
        mod1_UI("mod1"),
        mod2_UI("mod2")),

    rightsidebar = rightSidebar(),
    title = "Test",
)

server <- function(input, output, session) {
    shared = reactiveValues()
    callModule(mod1, "mod1",  shared)
    callModule(mod2, "mod2",  shared)
}
shinyApp(ui, server)
