library(shiny)
my_sal <- readRDS("sal.rds")
source("wf_wf_helper.R")
addResourcePath("test", ".")
ns <- NS("wf-wf")


ui <- fluidPage(
  (shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = ""),
    shinydashboardPlus::dashboardSidebar(),
    shinydashboard::dashboardBody()) %>% htmltools::findDependencies())[4:5],
  spsDepend("toastr"),
  shinyjs::useShinyjs(),
  tags$script(src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"),
  tags$script(src="test/wf_wf.js"),
  tags$link(rel="stylesheet", href="test/wf_wf.css"),
  br(), br(),br(),
  fluidRow(
    shinydashboardPlus::box(
      width = 6, id = ns("step_container"), title = "Workflow step designer",
      solidHeader = TRUE, status = "primary",
      div(
        id = ns("step_box"),
        animateIcon(
          id=ns("step_enlarge"),"expand-arrows-alt", animation = "pulse",
          size = "lg", class= "enlarge-icon",
          hover = TRUE, enlarge_target= paste0('#', ns('step_box')), enlarged='false'
        ) %>%
          bsTip("Enlarge the step designer", "bottom", "info"),
        uiOutput(ns("sort_box")),
        br(), br(), br(), spsHr(other_color = "rgb(2, 117, 216, 0.5)"),
        div(
          class = 'step-box-control',
          tags$button(class="fa fa-undo-alt shiny-bound-input action-button", id=ns("step_undo"), style="color: #3c8dbc") %>%
            bsTip("Undo", placement = "bottom"),
          tags$button(class="fa fa-redo-alt shiny-bound-input action-button", id=ns("step_redo"), style="color: #3c8dbc") %>%
            bsTip("Redo", placement = "bottom"),
          div(
            class = "wf-history-panel",
            tags$table(
              tags$tr(tags$td("Last history:"), tags$td("1")),
              tags$tr(tags$td("Current history:"), tags$td("2")),
              tags$tr(tags$td("Next history:"), tags$td("3"))
            )
          ),
          tags$i(class="fa fa-plus shiny-bound-input action-button", id=ns("step_new"), style="color: #5cb85c;") %>%
            bsTip("Add a new step", placement = "bottom", status = "success"),
          div(id=ns("step_trash"), class="step-trash", tags$span(), tags$i()) %>%
            bsTip("Drag here to delete a step", placement = "bottom", status = "danger"),
          tags$button(class="fa fa-save shiny-bound-input action-button", id=ns("totask"))%>%
          bsPop("Add to SPS workflow task", placement = "bottom",
                "Send the workflow to SPS workflow module manager so you can run it. You must save it first before add to task.")
        )
      )
    ),
    shinydashboardPlus::box(
      width = 6, id = ns("wf_plot_container"), title = "Workflow Dependency Plot",
      solidHeader = TRUE, status = "primary",
      div(
        id =ns("wf_plot_box"),
        animateIcon(
          id=ns("wf_plot_enlarge"),"expand-arrows-alt", animation = "pulse",
          size = "lg", class= "enlarge-icon",
          hover = TRUE, enlarge_target=paste0('#', ns('wf_plot_box')), enlarged='false'
        ) %>%
          bsTip("Enlarge workflow plot", "bottom", "info"),
        systemPipeR::plotwfOutput(outputId = ns("wf_plot")), br(), br()
      ),
      heightMatcher(ns("wf_plot_container"), ns("step_container"))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(
    "wf-wf",
    function(input, output, session) {
      ns <- session$ns
      renderSort <- renderUI({
        sal <- his$get()$item$sal
        sal_stat <- statSal(sal)
        destoryOb(wf_share$config_ob)
        # wf_share$config_ob <- makeConfig(sal, sal_stat$names, sal_stat$deps, session, input, output, ns)
        makeSort(sal_stat$names, sal_stat$type, ns)
      })
      # init ----
      ### dev shortcut ####
      shared <- reactiveValues()
      observeEvent(1, {
        shared$wf$sal <- my_sal
        wf_share$config_ob <- NULL
        his$add(list(
          sal = shared$wf$sal,
          msg = "Initial sal"
        ))
        savehis(savehis() + 1)
      }, once = TRUE)
      ########
      wf_share <- reactiveValues()
      # start history stack
      his <- historyStack$new(verbose = spsOption("verbose"),limit = 100)
      # save envet trigger
      savehis <- reactiveVal(0)
      observeEvent(1, once = TRUE, priority = 99L, {
        # # config obs
        # wf_share$config_ob <- NULL
        # his$add(list(
        #   sal = shared$wf$sal,
        #   msg = "Initial sal"
        # ))
      })
      # plot wf first time----
      # output$wf_plot <- systemPipeR::renderPlotwf({
      #   systemPipeR::plotWF(his$get()$item$sal, rstudio = TRUE)
      # })
      # # render sort  box first time ----
      # output$sort_box <-  renderUI({
      #   sal <- his$get()$item$sal
      #   sal_stat <- statSal(sal)
      #   print(1111)
      #   destoryOb(isolate(wf_share$config_ob))
      #   wf_share$config_ob <- makeConfig(sal, sal_stat$names, sal_stat$deps, session, input, output, ns)
      #   makeSort(sal_stat$names, sal_stat$type, ns)
      # })


      # new step ----
      observeEvent(input$step_new, {
        req(input$step_new)
        newStepMain(his$get()$item$sal, ns)
      }, ignoreInit = TRUE)
      # render modal based on new step type ----
      observeEvent(input$new_step_choose, ignoreInit = TRUE, {
        req(input$new_step_choose)
        req(input$new_step_type)
        req(input$new_step_index)
        sal <- his$get()$item$sal
        sal_stat <- statSal(sal)
        dep_choice <- sal_stat$names[seq(as.numeric(input$new_step_index))]

        if(input$new_step_type == "r") {
          showModal(modalDialog(
            size = "l",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("new_step_back"), "Back"),
              actionButton(ns("new_step_r_save"), "Save"),
            ), {
              div(
                spsTitle("Type the R code of this step below"),
                shinyAce::aceEditor(
                  ns("new_code_r"), fontSize = 14, value = "",
                  mode = "r", wordWrap = TRUE, debounce = 100, height = "250px",
                  placeholder = "Write some R code"
                ),
                spsTitle("Type step name"),
                p("Must be different than these names:"),
                tags$pre(paste(sal_stat$names, collapse = ", ")),
                textInput(
                  ns("new_step_name"), "",
                  paste0("step", input$new_step_index, "_", sample(letters, 3) %>% paste0(collapse = ""))
                ),
                spsTitle("Choose dependency"),
                p("This step will be inserted to the index you selected.
                  You may change the order later but remember to fix the dependency. ",
                  tags$span(class="text-danger", "Adding a downstream step as dependency is not allowed.")),
                selectizeInput(
                  ns("new_step_dep"), "", multiple = TRUE, choices = dep_choice[seq_len(length(dep_choice) - 1)],
                  selected = dep_choice[length(dep_choice) - 1]
                )
              )
          }))
        } else {
          showModal(modalDialog(
            size = "l",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("new_step_back"), "Back"),
              actionButton(ns("new_step_sys_save"), "Save"),
            ), {
              tabsetPanel(
                tabPanel(
                  "Basic Arguments",
                  spsTitle("Choose targets connection"),
                  p("It can be a previous step(s) or comes from a fresh file"),
                  shinyWidgets::radioGroupButtons(
                    justified = TRUE,
                    ns("new_sys_t_source"), "", choices = c(`Previous Step`="step", `New File`="upload"), status = "primary",
                    checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                  ),
                  conditionalPanel(
                    "input.new_sys_t_source == 'step'", ns = ns,
                    selectizeInput(
                      ns("new_step_t_con"), "Choose targets connection from a step(s)", multiple = TRUE, choices = dep_choice[seq_len(length(dep_choice) - 1)]
                    )
                  ),
                  conditionalPanel(
                    "input.new_sys_t_source == 'upload'", ns = ns,
                    dynamicFile(ns("new_sys_t_path"), "Choose new targets file from server", mode = "local")
                  ),
                  DT::DTOutput(ns("step_sys_t_df")),
                  spsTitle("Type step name"),
                  p("Must be different than these names:"),
                  tags$pre(paste(sal_stat$names, collapse = ", ")),
                  textInput(
                    ns("new_step_name"), "",
                    paste0("step", input$new_step_index, "_", sample(letters, 3) %>% paste0(collapse = ""))
                  ),
                  spsTitle("Choose dependency"),
                  p("This step will be inserted to the index you selected.
                  You may change the order later but remember to fix the dependency. ",
                  tags$span(class="text-danger", "Adding a downstream step as dependency is not allowed.")),
                  selectizeInput(
                    ns("new_step_dep"), "", multiple = TRUE, choices = dep_choice[seq_len(length(dep_choice) - 1)],
                    selected = dep_choice[length(dep_choice) - 1]
                  ),
                  spsTitle("sub-directory"),
                  p("Create a sub-dir inside results to store outputs from this step?"),
                  shinyWidgets::awesomeCheckbox(ns("new_sys_dir"), "sub-directory?", value = TRUE, status = "primary"),
                ),
                tabPanel(
                  "CWL Arguments",
                  spsTitle("Choose CWL file"),
                  selectInput(
                    ns("new_sys_cwl"), "", multiple = FALSE,
                    choices = fs::dir_ls("param/cwl", recurse = TRUE, regexp = "\\.cwl$") %>% str_remove("param/cwl/")
                  ),
                  spsTitle("Choose input yaml file for CWL"),
                  selectInput(
                    ns("new_sys_yaml"), "", multiple = FALSE,
                    choices = fs::dir_ls("param/cwl", recurse = TRUE, regexp = "\\.y[a]{0,}ml$") %>% str_remove("param/cwl/")
                  ),
                  spsTitle("Targets - Yaml replacement"),
                  p("You need to have targets table displayed in the `Basic Arguments`
                    tab and the proper yaml file (some yaml files have no varaibles)
                    to have the options shown below.", class="text-warning"),
                  tags$ul(
                    tags$li(id = ns("require_t_con"), class="text-danger", "Require valid target connections"),
                    tags$li(id = ns("require_yaml"), class="text-danger", "Require a yaml file with inputVar variables")
                  ),
                  p("Choose a column from a targets to replace a variables in CWL yaml you have chosen"),
                  fluidRow(id = ns("cwl_var")),
                  spsHr(),
                  p("Render to see raw Commandline"),
                  actionButton(ns("step_sys_parse"), "Render", class="center-block")
                ),
                tabPanel(
                  "Raw commandline",
                  verbatimTextOutput(ns("new_step_sys_cmd"))
                )
              )
            }))
        }
      })
      # back is clicked ----
      observeEvent(input$new_step_back, ignoreInit = TRUE, {
        newStepMain(his$get()$item$sal, ns)
      })
      # sys step ----
      new_step_sys_t_path <- dynamicFileServer(input,session, id = "new_sys_t_path", mode = "local")
      sys_t_con <- reactive({
        req(input$new_sys_t_source)
        if(input$new_sys_t_source == "step") return(input$new_step_t_con)
        new_step_sys_t_path()$datapath
      }) %>% debounce(2000)
      sys_bind_df <- reactiveVal()
      cwl_input_vars <- reactive({
        req(input$new_sys_yaml)
        file_content <- readLines(file.path("param", "cwl", input$new_sys_yaml))
        file_content %>%
          str_split("\n") %>%
          unlist() %>%
          {.[str_detect(., ".*:")]} %>%
          {.[str_detect(., ":\\s{0,}_[a-zA-Z0-9_]+_\\s{0,}$")]} %>%
          str_extract("_[a-zA-Z0-9_]+_")
      }) %>% debounce(1000)

      observeEvent(sys_t_con(), ignoreInit = TRUE, ignoreNULL = FALSE, {
        if(is.null(input$new_step_t_con)) return({
          output$step_sys_t_df <- DT::renderDT({DT::datatable(data.frame())})
          sys_bind_df(NULL)
        })
        sal <- his$get()$item$sal
        sal_stat <- statSal(sal)
        shinyCatch(blocking_level = "error", {
          if(input$new_sys_t_source == "step") {
            ## handle outfiles
            outfiles <- systemPipeR::outfiles(sal)[sys_t_con()] %>%
              lapply(as.data.frame) %>%
              {.[lapply(., function(x) nrow(x) > 0) %>% unlist()]}
            if(length(outfiles) > 1) {
              outfiles_length <- lapply(outfiles, nrow) %>% unlist()
              if(
                (length(unique(outfiles_length)) > 2) ||
                (outfiles_length[1] != mean(outfiles_length)) &&
                (!1 %in% outfiles_length)
              ) {
                stop("Steps you selected have different Sample length in outfiles, cannot use these steps as targets connections")
              }
            }
            outfiles <- dplyr::bind_cols(outfiles)
            ## handle targets
            targets <- systemPipeR::targetsWF(sal)[sys_t_con()] %>%
              lapply(as.data.frame) %>%
              {.[lapply(., function(x) nrow(x) > 0) %>% unlist()]}
            if(length(targets) > 1) {
              targets_length <- lapply(targets, nrow) %>% unlist()
              if(
                (length(unique(targets_length)) > 2) ||
                (targets_length[1] != mean(targets_length)) &&
                (!1 %in% targets_length)
              ) {
                stop("Steps you selected have different Sample length in targets, cannot use these steps as targets connections")
              }
              targets <- lapply(seq_along(targets), function(x){
                if(x == 1) return(targets[[x]])
                names(targets[[x]]) <- paste0(names(targets[[x]]), "_", names(targets)[x])
                targets[[x]]
              })
            }
            targets <- dplyr::bind_cols(targets)
            ## merge both
            df <-  if(length(outfiles) == 0 && length(targets) != 0) {
              targets
            } else if(length(outfiles) != 0 && length(targets) == 0) {
              outfiles
            } else if(length(outfiles) == 0 && length(targets) == 0) {
              stop("Selected steps don't have any targets or outfiles, try to choose other steps as targets connections")
            } else {
              if((nrow(targets) != nrow(outfiles)) && (nrow(outfiles) != 1)) {
                stop("Step(s) you selected have different length in",
                     "outfiles and targets dataframes or the outfiles",
                     "length is not 1, this is not allowed")
              }
              dplyr::bind_cols(targets, outfiles)
            }
          } else {
            df <- read.delim(sys_t_con(), comment.char = "#", sep = "\t")
          }
          output$step_sys_t_df <- DT::renderDT({DT::datatable(df, options = list(searching= FALSE, scrollX = TRUE), class = "compact")})
          sys_bind_df(df)
          shinyCatch(message("New targets table created."))
        })
      })

      observeEvent(c(cwl_input_vars(), sys_bind_df()),{
        shinyjs::toggleElement("require_t_con", condition = !emptyIsFalse(sys_bind_df()), anim = TRUE)
        shinyjs::toggleElement("require_yaml", condition = !emptyIsFalse(cwl_input_vars()), anim = TRUE)
        req(emptyIsFalse(cwl_input_vars()))
        req(emptyIsFalse(sys_bind_df()))
        removeUI(
          selector = glue('#{ns("cwl_var")} div'),
          multiple = TRUE
        )
        for(i in seq_along(cwl_input_vars())){
          insertUI(
            glue('#{ns("cwl_var")}'),
            where = "beforeEnd",
            ui =  column(3, selectizeInput(
              inputId = ns(paste0("cwl_var-", i)),
              label = cwl_input_vars()[i],
              choices = names(sys_bind_df()),
              options = list(style = "btn-primary")
            ))
          )
        }
        shinyCatch(message("New inputVar options created."))
      })

      observeEvent(input$step_sys_parse, ignoreInit = TRUE, {
        req(input$new_sys_cwl)
        args <- shinyCatch({
          # parse replacement
          replace_cols <- lapply(seq_along(cwl_input_vars()), function(i){
            input[[paste0("cwl_var-", i)]]
          }) %>% unlist()
          inputvars <- cwl_input_vars()
          names(inputvars) <- replace_cols
          # parsing
          systemPipeR::SYSargsList(
            targets = sys_t_con(), dir = input$new_sys_dir,
            wf_file = input$new_sys_cwl,
            input_file=input$new_sys_yaml,
            dir_path="param/cwl",
            inputvars=if(emptyIsFalse(inputvars)) inputvars else NULL,
            step_name = input$new_step_name,
            dependency = if(is.null(input$new_step_dep)) "" else input$new_step_dep
          )
        }, blocking_level = "error")
        output$new_step_sys_cmd <- renderPrint({
          systemPipeR::cmdlist(args) %>% unlist() %>% unname()
        })
        shinyCatch(message("Raw commandline code generated."))
      })
      # sys step new save ----
      observeEvent(input$new_step_sys_save, {
        req(input$new_step_r_save)
        sal <- his$get()$item$sal
      }, ignoreInit = TRUE)

      # R step new save ----
      observeEvent(input$new_step_r_save, {
        req(input$new_step_r_save)
        sal <- his$get()$item$sal
        shinyCatch(blocking_level = "error", {
          if(!emptyIsFalse(input$new_code_r)) stop("R code is empty")
          if(!emptyIsFalse(input$new_step_name)) stop("Step name is empty")
          options(linewise_importing = TRUE)
          systemPipeR::appendStep(sal, after = as.numeric(input$new_step_index) - 1) <- systemPipeR::LineWise(
            code = input$new_code_r, step_name = input$new_step_name,
            dependency = if(is.null(input$new_step_dep)) "" else input$new_step_dep
          )
          his$add(list(sal = sal, msg = "New R step"))
        })
        savehis(savehis() + 1)
        removeModal()
        shinyCatch(message("New R step saved."))
      }, ignoreInit = TRUE)

      # step config save ---
      ## config R
      observeEvent(input$save_config_r, {
        req(input$save_config_r)


        his$add(list(sal = sal, msg = "Config R step"))
        wf_ui_updates()
        updateHisInfo(his)
      })
      ## config sys
      observeEvent(input$save_config_sys, {
        req(input$save_config_sys)
        sal <- his$get()$item$sal

        mysal@stepsWF$load_SPR@codeLine

        his$add(list(sal = sal, msg = "Config sysArgs step"))
        wf_ui_updates()
        updateHisInfo(his)
      })


      # when new sal history  ----
      wf_ui_updates <- function(){
        sal <- his$get()$item$sal
        sal_stat <- statSal(sal)
        # update sort
        output$sort_box <-  renderUI({
          destoryOb(isolate(wf_share$config_ob))
          wf_share$config_ob <- makeConfig(sal, sal_stat$names, sal_stat$deps, session, input, output, ns)
          makeSort(sal_stat$names, sal_stat$type, ns, sal_stat)
        })
        # update plot
        output$wf_plot <- systemPipeR::renderPlotwf({
          systemPipeR::plotWF(sal, rstudio = TRUE)
        })
        # update redo undo
        shinyjs::toggleState("step_undo", !his$status()$first)
        shinyjs::toggleState("step_redo", !his$status()$last)
      }
      observeEvent(savehis(), {
        req(savehis() > 0)
        wf_ui_updates()
        updateHisInfo(his)
      })
      # on sort order change ----
      step_order_inited <- reactiveVal(FALSE)
      step_order_triggered <- reactiveVal(FALSE)
      observeEvent(input$step_orders, {
        req(input$step_orders)
        if(!step_order_inited()) return(step_order_inited(TRUE))
        if(step_order_triggered()) return(step_order_triggered(FALSE))
        sal <- his$get()$item$sal
        new_sal <- sal[as.numeric(input$step_orders)]
        his$add(list(sal = new_sal, msg = "Change order"))
        savehis(savehis() + 1)
        step_order_triggered(TRUE)
      }, ignoreInit = TRUE)
      # on redo or undo ----
      updateHisInfo <- function(his){
        getMsg <- function(pos){
          quiet(shinyCatch(his$get(pos)$item$msg, shiny = FALSE)) %>%
            if(emptyIsFalse(.)) . else "some action"
        }
        stats <- his$status()
        msgs <- paste0(stats$pos, ".", getMsg(stats$pos))
        msgs <- if(!stats$first) c(paste0(stats$pos - 1, ". ", getMsg(stats$pos - 1)), msgs) else c("-", msgs)
        msgs <- if(!stats$last) c(msgs, paste0(stats$pos + 1, ". ", getMsg(stats$pos + 1))) else c(msgs,  "-")
        session$sendCustomMessage("wf-undo-redo", list(msg = unname(msgs)))
      }
      observeEvent(input$step_undo, {
        req(!his$status()$first)
        shinyCatch(his$backward(), blocking_level = "error")
        wf_ui_updates()
        updateHisInfo(his)
      })
      observeEvent(input$step_redo, {
        req(!his$status()$last)
        shinyCatch(his$forward(), blocking_level = "error")
        wf_ui_updates()
        updateHisInfo(his)
      })
      # on final save button ----

    }
  )
}
shinyApp(ui, server)
