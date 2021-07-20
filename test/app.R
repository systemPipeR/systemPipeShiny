library(shiny)
sal <- readRDS("sal.rds")
#' @rdname plotwf-shiny
# #' @export
# renderPlotwf <- function(expr, env = parent.frame(), quoted = FALSE) {
#   if (!quoted) { expr <- substitute(expr) } # force quoted
#   htmlwidgets::shinyRenderWidget(expr, plotwfOutput, env, quoted = TRUE)
# }
#
# plotwfOutput <- function(outputId, width = '100%', height = '400px'){
#   htmlwidgets::shinyWidgetOutput(outputId, 'plotwf', width, height, package = 'systemPipeR')
# }

makeSort <- function(sal_names, step_type){
  tagList(
    div(
      id = "sortable",
      lapply(seq_along(sal_names), function(x)
        div(
          class = "step-grid", tabindex = "-1", `data-id`=x,
          tags$span(paste0(step_type[x], " Step ", x, ": ", sal_names[x])),
          actionButton(paste0("configure", x), "", title="config this step", icon = animateIcon("cog", color = "rgb(2, 117, 216, 0.5)"))
        )
      )
    ), br(), br(), br(), spsHr(other_color = "rgb(2, 117, 216, 0.5)"),
    div(
      class = 'step-box-control',
      tags$i(class="fa fa-redo-alt shiny-bound-input action-button", id="step_cancel", style="color: #f39c12") %>%
        bsPop("Resume to last saved state", "give up current changes and go back to the initial state or the last time you clicked save."),
      tags$i(class="fa fa-plus shiny-bound-input action-button", id="step_new", style="color: #5cb85c;") %>%
        bsTip("Add a new step", placement = "bottom", status = "success"),
      tags$i(class="fa fa-save shiny-bound-input action-button", id="step_save") %>%
        bsTip("Save all modifications"),
      div(id="step_trash", class="step-trash", tags$span(), tags$i()) %>%
        bsTip("Drag here to delete a step", placement = "bottom", status = "danger")
    ),
    tags$script(src="test/wf_wf_sort.js")
  )
}

makeRmodal <- function(index, step, deps, step_name, sal_names) {
  step_code <- as.character(sal$stepsWF[[15]]$codeLine)
  tabsetPanel(
    tabPanel(
      "Basic Info",
      spsTitle("Object", "4"),
      tags$pre(glue(
        "Step: ", step_name, "\n",
        glue_collapse(capture.output(step) %>% remove_ANSI(), sep = "\n")
      )),
      spsTitle("Dependency", "4"),
      selectizeInput(
        paste0("change_dep", index), "", multiple = TRUE,
        selected = deps[[index]], choices = sal_names[seq_len(index -1)] %>% unlist() %>% unname() %>% na.omit()
      ) %>% bsPop("Change Dependency", "You can only choose steps before this step as
                        dependencies.", placement = "right")
    ),
    tabPanel(
      "R code",
      shinyAce::aceEditor(
        paste0("edit_code", index), fontSize = 14,
        step_code, "r", wordWrap = TRUE, debounce = 10,
      )
    )
  )
}

makeSysModal <- function(index, step, deps, step_name, sal_names, sal){
  cmd <- unlist(lapply(systemPipeR::cmdlist(sal[index])[[1]], function(x) glue_collapse(unname(unlist(x)), sep = "\n")))
  cmd_str <- paste(paste0("\n#", names(cmd)), cmd, sep="\n", collapse = "\n")
  targets_header <- step$targetsheader
  targets_header <- if(emptyIsFalse(targets_header)) targets_header[[1]] else ""
  tabsetPanel(
    tabPanel(
      "Basic Info",
      spsTitle("Object", "4"),
      tags$pre(glue(
        "Step: ", step_name, "\n",
        glue_collapse(capture.output(step) %>% remove_ANSI(), sep = "\n")
      )),
      spsTitle("Dependency", "4"),
      selectizeInput(
        paste0("change_dep", index), "", multiple = TRUE,
        selected = deps[[index]], choices = sal_names[seq_len(index -1)] %>% unlist() %>% unname() %>% na.omit()
      ) %>% bsPop("Change Dependency", "You can only choose steps before this step as
                        dependencies.", placement = "right"),
      spsTitle("Tools required", "4"),
      tags$pre(glue_collapse(step$modules %>% unlist() %>% unname(), sep = "\n")),
      spsTitle("File paths", "4"),
      tags$pre(glue_collapse(c(
        "#CWL file",
        step$files$cwl,
        "\n#CWL input yaml path",
        step$files$yml,
        "\n#sub-step CWL path(s)",
        step$files$cltpaths,
        "\n#targets file path if this step is loaded from a file",
        if(emptyIsFalse(step$files$targets)) step$files$targets else ""),
        sep = "\n"
      ))
    ),
    tabPanel(
      "CMD",
      spsTitle("sysArgs step commandline code", "4"),
      shinyAce::aceEditor(
        "display_cmd", cmd_str, "sh", readOnly = TRUE, fontSize = 14, wordWrap = TRUE
      )
    ),
    tabPanel(
      "Targets", style = "overflow-x: auto;",
      spsTitle("Choose targets connection", "4"),
      selectizeInput(
        paste0("dddddd", index), "", multiple = TRUE,
        selected = deps[[index]], choices = sal_names[seq_len(index -1)] %>% unlist() %>% unname() %>% na.omit()
      ) %>% bsPop("Change Targets Connections", "If this step requires info from
                        previous step(s)' targets/outfiles columns, specify here",
                  placement = "right"),
      spsTitle("Targets header", "4"),
      tags$pre(glue_collapse(targets_header, sep = "\n")),
      spsTitle("Targets table", "4"),
      DT::DTOutput(paste0("targets", index))
    ),
    tabPanel(
      "Outfiles",  style = "overflow-x: auto;",
      spsTitle("sysArgs step outfiles table", "4"),
      DT::DTOutput(paste0("outfiles", index))
    )
  )
}


makeConfig <- function(sal, sal_names, deps, session, input, output) {
  lapply(seq_along(sal$stepsWF), function(index){
    is_r_step <- inherits(sal$stepsWF[[index]], "LineWise")
    modal <- if (is_r_step) {
      makeRmodal(index, sal$stepsWF[[index]], deps, sal_names[index], sal_names)
    } else {
      makeSysModal(index, sal$stepsWF[[index]], deps, sal_names[index], sal_names, sal)
    }
    observeEvent(input[[paste0("configure", index)]], {
      req(input[[paste0("configure", index)]])
      showModal(modalDialog(
        modal, title = paste0("Configure ", sal_names[index]), size = "l", footer = tagList(
          modalButton("Cancel"),
          actionButton("save", "Save")
        )
      ))
      req(!is_r_step)
      outfiles_df <- as.data.frame(systemPipeR::outfiles(sal[index])[[1]])
      targets_df <- as.data.frame(targetsWF(sal[index])[[1]])
      output[[paste0("targets", index)]] <- DT::renderDT({DT::datatable(targets_df, options = list(searching= FALSE), class = "compact")})
      output[[paste0("outfiles", index)]]<- DT::renderDT({DT::datatable(outfiles_df, options = list(searching= FALSE), class = "compact")})

    }, ignoreInit = TRUE)
  })
}



addResourcePath("test", ".")
ui <- fluidPage(
  (shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = ""),
    shinydashboardPlus::dashboardSidebar(),
    shinydashboard::dashboardBody()) %>% htmltools::findDependencies())[4:5],
  spsDepend("toastr"),
  tags$script(src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"),
  tags$script(src="test/wf_wf.js"),
  tags$link(rel="stylesheet", href="test/wf_wf.css"),
  br(), br(),br(),
  fluidRow(
    box(
      width = 6, id = "step_container", title = "Workflow step designer",
      solidHeader = TRUE, status = "primary",
      div(
        animateIcon(
          id="step_enlarge","expand-arrows-alt", animation = "pulse",
          size = "lg", class= "enlarge-icon",
          hover = TRUE, enlarge_target='#step_box', enlarged='false'
        ) %>%
          bsTip("Enlarge the step designer", "bottom", "info"),
        uiOutput("step_box"),
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
        systemPipeR::plotwfOutput(outputId = "wf_plot"), br(), br()
      ),
      heightMatcher("wf_plot_container", "step_container")
    )
  ),
  fluidRow(
    box(
      title = "Workflow designer actions",
      status = "primary",
      class = "step-control-panel",
      width = 12,
      solidHeader = TRUE,
      div(
        style="color: #00a65a; font-size: 2rem",
        tags$b("Next/Add to task: "),
        actionButton("totask", "", icon = animateIcon("check"), style="font-size: 2rem")
      ) %>%
        bsPop("Add to SPS workflow task", placement = "bottom",
              "Send the workflow to SPS workflow module manager so you can run it. You must save it first before add to task.")
    )
  ),

)

server <- function(input, output, session) {
  output$wf_plot <- systemPipeR::renderPlotwf({
    req(wf_share$sal)
    # systemPipeR::plotWF(sal, no_plot = TRUE, out_format = "dot_print")
    systemPipeR::plotWF(sal, rstudio = TRUE)
  })

  observeEvent(input$step_new, {
    req(input$step_new)
    ask_confirmation(
      inputId = "new_step_confirm",
      title = "R step or sysArgs step",
      text = shinyWidgets::radioGroupButtons(
        "new_step_type", "", choices = c(`R step`="r", `sysArgs Step`="sys"), status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      btn_labels = c("Cancel", "Next"),
      html = TRUE
    )
  })

  observeEvent(input$new_step_confirm, {
    req(input$new_step_confirm)
    req(input$new_step_type)
    if(input$new_step_type == "r") {
      showModal(modalDialog(size = "l", footer = tagList(modalButton("Cancel"), actionButton("save", "Save")), {
        shinyAce::aceEditor(
          "edit_new_r", fontSize = 14, value = "# Type your R code for this step",
          mode = "r", wordWrap = TRUE, debounce = 10
        )
      }))
    }
  })

  wf_share <- reactiveValues()
  observeEvent(1, once = TRUE, priority = 99L, {
    wf_share$sal <- sal
    wf_share$sal_names <-  names(wf_share$sal$stepsWF)
    wf_share$step_type <-  unlist(lapply(sal$stepsWF, function(x){if(inherits(x, "LineWise")) "R" else "sysArgs"}))
    wf_share$deps <- wf_share$sal$dependency
    wf_share$config_ob <- NULL
  })

  output$step_box <-  renderUI({
    makeConfig(wf_share$sal, wf_share$sal_names, wf_share$deps, session, input, output)
    makeSort(wf_share$sal_names, wf_share$step_type)
  })
  observeEvent(input$step_save, {
    lapply(wf_share$config_ob, function(x) x$destroy())
    wf_share$config_ob <- makeConfig(wf_share$sal, wf_share$sal_names, wf_share$deps, session, input, output)
    output$step_box <-   renderUI({makeSort(wf_share$sal_names, wf_share$step_type)})
  })

  observeEvent(input$step_save, shinyCatch(blocking_level = "error", {
    new_sal <- isolate(wf_share$sal)[as.numeric(input[['wf-wf-step_orders']])]
    # if(is.null())
    wf_share$sal <- isolate(wf_share$sal)
    wf_share$sal_names <-  names(wf_share$sal$stepsWF)
    wf_share$step_type <-  unlist(lapply(sal$stepsWF, function(x){if(inherits(x, "LineWise")) "R" else "sysArgs"}))
    wf_share$deps <- wf_share$sal$dependency
    wf_share$config_ob <- NULL
    lapply(wf_share$config_ob, function(x) x$destroy())
    wf_share$config_ob <- makeConfig(wf_share$sal, wf_share$sal_names, wf_share$deps, session, input, output)
    # print(wf_share$sal)
    output$wf_plot <- systemPipeR::renderPlotwf({
      systemPipeR::plotWF(wf_share$sal, rstudio = TRUE)
      # plot(1)
    })
  }))

}
system.file("extdata", package = "systemPipeR")
shinyApp(ui, server)
