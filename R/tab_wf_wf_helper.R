makeSort <- function(sal_names, step_type, ns, sal_stat, ob_index){
    # check dependencies
    classes <- wfCheckDep(sal_stat, session)
    # create list
    tagList(
        div(
            id = ns("sortable"),
            lapply(seq_along(sal_names), function(x)
                div(
                    class = classes[x], tabindex = "-1", `data-id`=x,
                    tags$span(paste0(step_type[x], " Step ", x, ": ", sal_names[x])),
                    actionButton(ns(paste0("configure", ob_index(), "_", x)), "", title="config this step", icon = animateIcon("cog", color = "rgb(2, 117, 216, 0.5)"))
                )
            )
        ),
        if(length(unique(classes)) > 1) {
            tagList(
                tags$span("Please fix dependency of red steps", style="background-color: rgb(255, 104, 115, 0.25);"),
                tags$script('$("#wf-wf-totask").prop(\'disabled\', true);')
            )
        } else tags$script('$("#wf-wf-totask").prop(\'disabled\', false);'),
        tags$script(src="sps/js/sps_wf_wf_sort.js")
    )
}

makeRmodal <- function(index, step, deps, step_name, sal_names, ns) {
    step_code <- as.character(step$codeLine)
    tabsetPanel(
        tabPanel(
            "Basic Info",
            spsTitle("Step Name", "4"),
            p("Step name cannot duplicate following and can only be letters, numbers and underscore _, no space."),
            tags$pre(paste(collapse = ", ", sal_names %>% {.[!. %in% step_name]})),
            textInput(ns("change_name"), "", value = step_name),
            spsTitle("Step Object", "4"),
            tags$pre(glue(
                "Step: ", step_name, "\n",
                glue_collapse(capture.output(step) %>% remove_ANSI(), sep = "\n")
            )),
            spsTitle("Dependency", "4"),
            p("No choice will be given if first step."),
            selectizeInput(
                ns("change_dep"), "", multiple = TRUE,
                selected = deps[[index]], choices = sal_names[seq_len(index -1)] %>% unlist() %>% unname() %>% na.omit()
            ) %>% bsPop("Change Dependency", "You can only choose steps before this step as
                        dependencies.", placement = "right", trigger = "hover")
        ),
        tabPanel(
            "R code",
            shinyAce::aceEditor(
                ns("edit_code"), fontSize = 14,
                step_code, "r", wordWrap = TRUE, debounce = 10,
            )
        )
    )
}

makeSysModal <- function(index, step, deps, step_name, sal_names, sal, ns){
    cmd <- unlist(lapply(systemPipeR::cmdlist(sal[index])[[1]], function(x) glue_collapse(unname(unlist(x)), sep = "\n")))
    cmd_str <- paste(paste0("\n#", names(cmd)), cmd, sep="\n", collapse = "\n")
    targets_header <- step$targetsheader
    targets_header <- if(emptyIsFalse(targets_header)) targets_header[[1]] else ""
    tabsetPanel(
        tabPanel(
            "Basic Info",
            spsTitle("Step Name", "4"),
            p("Step name cannot duplicate following and can only be letters, numbers and underscore _, no space."),
            tags$pre(paste(collapse = ", ", sal_names %>% {.[!. %in% step_name]})),
            textInput(ns("change_name"), "", value = step_name),
            spsTitle("Step Object", "4"),
            tags$pre(glue(
                "Step: ", step_name, "\n",
                glue_collapse(capture.output(step) %>% remove_ANSI(), sep = "\n")
            )),
            spsTitle("Dependency", "4"),
            p("No choice will be given if first step."),
            selectizeInput(
                ns("change_dep"), "", multiple = TRUE,
                selected = deps[[index]], choices = sal_names[seq_len(index -1)] %>% unlist() %>% unname() %>% na.omit()
            ) %>% bsPop("Change Dependency", "You can only choose steps before this step as
                        dependencies.", placement = "right", trigger = "hover"),
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
                if(emptyIsFalse(step$files$targets)) step$files$targets
                else "Targets of this step is inherited from a previous step, see Targets tab/ or no targets required."),
                sep = "\n"
            ))
        ),
        tabPanel(
            "CMD",
            spsTitle("sysArgs step commandline code", "4"),
            p('Display code only, you are not allowed to change here.
              Create a new step with proper CWL and yaml to change the commandline code'),
            shinyAce::aceEditor(
                ns("display_cmd"), cmd_str, "sh", readOnly = TRUE, fontSize = 14, wordWrap = TRUE
            )
        ),
        tabPanel(
            "Targets", style = "overflow-x: auto;",
            spsTitle("Targets connection(inherited):", "4"),
            tags$pre(sal$targets_connection[[step_name]][['targets_step']]),
            spsTitle("Targets header", "4"),
            tags$pre(glue_collapse(targets_header, sep = "\n")),
            spsTitle("Targets table", "4"),
            DT::DTOutput(ns(paste0("targets", index)))
        ),
        tabPanel(
            "Outfiles",  style = "overflow-x: auto;",
            spsTitle("sysArgs step outfiles table", "4"),
            DT::DTOutput(ns(paste0("outfiles", index)))
        )
    )
}

destoryOb <- function(ob){
    try(lapply(ob, function(x) x$destroy()), silent = TRUE)
}

statSal <- function(sal){
    list(
        names = names(sal$stepsWF),
        index = seq_along(sal),
        len = length(sal),
        type = unlist(lapply(sal$stepsWF, function(x){if(inherits(x, "LineWise")) "R" else "sysArgs"})),
        deps = sal$dependency
    )
}

newStepMain <- function(sal, ns){
    sal_stat <- statSal(sal)
    showModal(modalDialog(
        size = "m",
        footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("new_step_choose"), "Next")
        ), {
            div(
                spsTitle("What kind of step to create"),
                shinyWidgets::radioGroupButtons(
                    justified = TRUE,
                    ns("new_step_type"), "", choices = c(`R step`="r", `sysArgs Step`="sys"), status = "primary",
                    checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                ),
                spsTitle("Where to insert the new step (index)?"),
                p("The number here means as step X. For exmaple,
                5 means original step 1-4 + new step 5 + original step 5-end. Default is insert after original last step."),
                selectInput(
                    ns("new_step_index"), "",
                    choices = c(sal_stat$index, sal_stat$len + 1),
                    selected = sal_stat$len + 1
                )
            )
        }))
}

makeConfig <- function(sal, sal_names, deps, session, input, output, ns, cur_config, ob_index) {
    lapply(seq_along(sal$stepsWF), function(index){
        is_r_step <- inherits(sal$stepsWF[[index]], "LineWise")
        modal <- if (is_r_step) {
            makeRmodal(index, sal$stepsWF[[index]], deps, sal_names[index], sal_names, ns)
        } else {
            makeSysModal(index, sal$stepsWF[[index]], deps, sal_names[index], sal_names, sal, ns)
        }
        ob_id <- paste0("configure", ob_index(), "_", index)
        observeEvent(input[[ob_id]], {
            req(input[[ob_id]])
            cur_config(index)
            showModal(modalDialog(
                modal, title = paste0("Configure ", sal_names[index]), size = "l", footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns(if(is_r_step) "save_config_r" else "save_config_sys"), "Save")
                )
            ))
            req(!is_r_step)
            outfiles_df <- as.data.frame(systemPipeR::outfiles(sal[index])[[1]])
            targets_df <- as.data.frame(sal[index]$targetsWF[[1]])
            output[[paste0("targets", index)]] <- DT::renderDT({DT::datatable(
                if(nrow(targets_df) != 0 ) targets_df else data.frame(`No targets for this step` = "No targets for this step"),
                options = list(searching= FALSE), class = "compact"
            )})
            output[[paste0("outfiles", index)]]<- DT::renderDT({DT::datatable(outfiles_df, options = list(searching= FALSE), class = "compact")})
        }, ignoreInit = TRUE)
    })
}

wfCheckDep <- function(sal_stat, session){
    classes <- rep("step-grid", sal_stat$len)
    dep_warn <- !lapply(sal_stat$index, function(i) {
        if(i == 1) {
            return(!emptyIsFalse(sal_stat$deps[[i]]))
        }
        all(sal_stat$deps[[i]] %in% sal_stat$names[seq_len(i -1)])
    }) %>% unlist()
    classes[dep_warn] <- "step-grid dep-warn"
    classes
}
