library(shiny)
sal <- readRDS("sal.rds")
plotwfOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plotwf', width, height, package = 'systemPipeR')
}

#' @rdname plotwf-shiny
# #' @export
renderPlotwf <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, plotwfOutput, env, quoted = TRUE)
}
addResourcePath("test", ".")
ui <- fluidPage(
  (shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = ""),
    shinydashboardPlus::dashboardSidebar(),
    shinydashboard::dashboardBody()) %>% htmltools::findDependencies())[4:5],
  tags$script(src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"),
  tags$script(src="test/wf_wf.js"),
  tags$link(rel="stylesheet", href="test/wf_wf.css"),
  fluidRow(
    box(
      title = "Workflow designer controls",
      status = "primary",
      class = "step-control-panel",
      width = 12,
      solidHeader = TRUE,
      div(
        tags$b("Save Modifications:"),
        actionButton("save_steps", "", icon = animateIcon("save"))
      ),
      div(
        class= "btn-group", role='group',
        tags$b("Other actions: "),
        actionButton("save_steps", "", icon = animateIcon("save")),
        actionButton("save_steps", "", icon = animateIcon("save")),
        actionButton("save_steps", "", icon = animateIcon("save")),
        actionButton("save_steps", "", icon = animateIcon("save"))
      ),
      div(
        tags$b("Confirm this section: ", style="color: #3c8dbc"),
        actionButton("save_steps", "", icon = animateIcon("save"))
      )
    )
  ),
  fluidRow(
    box(
      width = 6, id = "step_container", title = "Workflow step designer",
      solidHeader = TRUE, status = "primary",
      div(
        animateIcon(
          id="step_enlarge","expand-arrows-alt", animation = "pulse",
          size = "lg", class= "enlarge-icon",
          hover = TRUE, enlarge_target='#sortable', enlarged='false'
        ) %>%
          bsTip("Enlarge the step designer", "bottom", "info"),
        div(
          id = "sortable",
          lapply(1:20, function(x)
            div(
              class = "step-grid",
              tags$span("item", x, "ssss sssss ssssssssss sssssssss ssss ssss ssssssss dddddddddd"),
              actionButton(paste0("a", x), "", title="config this step", icon = animateIcon("cog", color = "rgb(2, 117, 216, 0.5)"))
            )
          )
        )
      )),
    box(
      width = 6, id = "wf_plot_container", title = "Workflow Plot",
      solidHeader = TRUE, status = "primary",
      div(
        id ="wf_plot_box",
        animateIcon(
          id="wf_plot_enlarge","expand-arrows-alt", animation = "pulse",
          size = "lg", class= "enlarge-icon",
          hover = TRUE, enlarge_target='#wf_plot_box', enlarged='false'
        ) %>%
          bsTip("Enlarge workflow plot", "bottom", "info"),
        plotwfOutput(outputId = "wf_plot")
      ),
      heightMatcher("wf_plot_container", "step_container")
    )
  ),

)

server <- function(input, output, session) {
  output$wf_plot <- renderPlotwf({
    # systemPipeR::plotWF(sal, no_plot = TRUE, out_format = "dot_print")
    systemPipeR::plotWF(sal, rstudio = TRUE, )
  })

}
system.file("extdata", package = "systemPipeR")
shinyApp(ui, server)
