library(shiny)
addResourcePath("test", ".")
ui <- fluidPage(
    tags$script(src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"),
    tags$script(src="test/wf_wf.js"),
    tags$link(rel="stylesheet", href="test/wf_wf.css"),
  column(6, div(
    animateIcon(
      id="step_enlarge","expand-arrows-alt", animation = "pulse",
      color = "rgb(2, 117, 216, 0.5)", size = "lg", class= "enlarge-icon",
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
  ))
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
