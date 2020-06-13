plotContainer <- R6::R6Class("plot_container",
    public = list(
        initialize = function(){
            if(not_empty(getOption('sps')$verbose)){
                if(getOption('sps')$verbose){
                    msg("Created a plot container R6 object")}
            }
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
            if(not_empty(self$plot_ui[[tab_id]]))
                msg(glue("Plot UI for this tab `{tab_id}` already exists ",
                         "in the container, overwrite"),
                    "warning")
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
            if(not_empty(self$plot_server[[tab_id]]))
                msg(glue("Plot server with id {tab_id} already exists ",
                             "in the container, overwrite"), "warning")
            self$plot_server[[tab_id]] <- list(func = render_func,
                                               args = list(isolate(expr_func()), ...))
            return(render_func(expr_func(), ...))
        },
        getServer = function(tab_id){
            render_expr <- self$plot_server[[tab_id]]
            if(is.null(render_expr))
                msg(glue("Can't find `{tab_id}` in plot_server list"), "error")
            return(do.call(render_expr$func, render_expr$args))
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
            if(is.null(attr_value)) msg(glue("Can't find attribute `{attrib}`"), "warning")
        }
    )
)

aaa = plotContainer$new()

mod1_UI <- function(id) {
  ns <- NS(id)
  tagList(
      actionButton(ns("render"), "render"),
      actionButton(ns("fire"), "fire"),
      jqui_resizable(aaa$addUI(plotlyOutput(ns("plot1")), id)),
      sliderInput(ns("slide"), label = "rows", min = 1, max = nrow(iris), value = nrow(iris))

  )
}

mod1 <- function(input, output, session, shared) {
    observeEvent(input$fire, {
        output$plot1 <- aaa$addServer(renderPlotly, 'mod1', {
            ggplotly(ggplot(iris[1:input$slide, ], aes(Sepal.Length, Sepal.Width)) +
                geom_point(aes(colour = Species)))
        })
        shared$signal <- c("mod1", input$fire)
    })
}
mod2_UI <- function(id){
    ns <- NS(id)
    tagList(
        actionButton(ns("a"), 'a'),
        uiOutput(ns("new"))
    )
}

mod2 <- function(input, output, session, shared) {
    ns <- session$ns
    make_plots <- reactiveValues(ui = list(), server = list())
    observeEvent(shared$signal, {
        tab_id <- shared$signal[1]
        new_plot_id <- glue("{tab_id}-{shared$signal[2]}")
        print(new_plot_id)
        make_plots$ui[[new_plot_id]] <-
            jqui_resizable(
                # boxPlus(width = 3, closable = TRUE, footer = "dsad", title = glue("Tab {tab_id} plot {shared$signal[2]}"),
                    aaa$getUI(tab_id, ns(new_plot_id))
                # )
            )
        print( make_plots$ui)
        make_plots$server[[new_plot_id]] <- aaa$getServer(tab_id)

        output$new <- renderUI({
            tagList(make_plots$ui)
        })
        for(plot_id in names(make_plots$server)){
            output[[plot_id]] <- make_plots$server[[plot_id]]
        }
    })
}
ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(    mod1_UI("mod1"),
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
