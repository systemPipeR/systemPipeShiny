## UI
admin_infoUI <- function(id){
    ns <- NS(id)
    tagList(
        tags$head(
            tags$script(src = "sps/js/sps_admin_info.js")
        ),
        tabTitle("App server information"),
        spsHr(),
        br(),
        fluidRow(
            shinydashboard::infoBoxOutput(ns("uptime"), width = 3),
            shinydashboard::infoBoxOutput(ns("cpu_now"), width = 3),
            shinydashboard::infoBoxOutput(ns("cpu_temp"), width = 3),
            shinydashboard::infoBoxOutput(ns("ram_now"), width = 3),
            shinydashboard::infoBoxOutput(ns("swap_now"), width = 3),
            shinydashboard::infoBoxOutput(ns("disk_now"), width = 3)
        ),
        spsHr(), h3("Details"),
        div(id = ns("detail_panel"), tabsetPanel(
            id = ns("info_plots"),
            tabPanel(
                "CPU Use", value = "cpu_use",
                h4("CPU Usage"),
                plotly::plotlyOutput(ns("cpu_use_plot"))
            ),
            tabPanel(
                "CPU Temp", value = "cpu_temp",
                h4("CPU Temperature"),
                plotly::plotlyOutput(ns("cpu_temp_plot"))
            ),
            tabPanel(
                "RAM", value = "ram_use",
                h4("RAM Usage"),
                plotly::plotlyOutput(ns("ram_plot"))
            )
        )),
        br()
    )
}



## server
admin_infoServer <- function(id, shared){
    # functions required ----
    timeDiffString <- function(){
        if(!exists("time_start")) return("NA")
        if(!inherits(time_start, "POSIXct")) return("NA")
        pass_time <- as.numeric(difftime(Sys.time(), time_start, units = "mins"))
        if(pass_time >= 60) pass_time <- as.numeric(difftime(Sys.time(), time_start, units = "hours"))
        else return(paste(round(pass_time), "mins"))
        if(pass_time >= 60) pass_time <- as.numeric(difftime(Sys.time(), time_start, units = "days"))
        else return(paste(round(pass_time), "hours"))
        return(paste(round(pass_time), "days"))
    }

    getCPU <- function(interval = 0.5){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        cpu_file <- system.file("app", "bash", "get_cpu.sh", package = "systemPipeShiny")
        cpu_now <- try(system(paste("bash", cpu_file, interval), intern = TRUE, timeout = 2))
        if (inherits(cpu_now, "try-error") || length(cpu_now) == 0) return("Cannot get it")
        cpu_strings <- stringr::str_split(cpu_now, "-")
        cpu_strings %>% unlist() %>%
            matrix(nrow=length(cpu_strings), byrow=TRUE) %>%
            {quiet(tibble::as_tibble(.))} %>%
            dplyr::mutate(V2 = as.numeric(V2))
    }
    cpuString <- function(cpu_df){
        if (is.character(cpu_df)) return(cpu_df)
        cpu <- round(cpu_df[1, 2], 2)
        color <- if (cpu > 80) "red" else if (cpu > 45) "orange" else "olive"
        return(c(paste0(cpu, "%"), color))
    }


    getRAM <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        ram_now <- try(system("free --mega | egrep \"Mem|Swap\" | awk '{print $3 \"\\n\" $2}'", intern = TRUE, timeout = 1))
        if (inherits(ram_now, "try-error") || length(ram_now) == 0) return("Cannot get it")
        return(as.numeric(ram_now))
    }
    ramString <- function(ram_now){
        if (is.character(ram_now)) return(ram_now)
        ram_frac <- ram_now[1] / ram_now[2]
        color <- if (ram_frac > 0.8) "red" else if (ram_frac > 0.45) "orange" else "olive"
        return(c(paste0(round(ram_now[1]/1024, 1), "G/", round(ram_now[2]/1024, 1), "G"), color))
    }
    swapString <- function(ram_now){
        if (is.character(ram_now)) return(ram_now)
        ram_frac <- ram_now[3] / ram_now[4]
        color <- if (ram_frac > 0.8) "red" else if (ram_frac > 0.45) "orange" else "olive"
        return(c(paste0(round(ram_now[3]/1024, 1), "G/", round(ram_now[4]/1024, 1), "G"), color))
    }

    diskString <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        app_path <- if(isFALSE(spsOption("app_path")))  getwd() else spsOption("app_path")

        disk_now <- try(system(paste0("du -sh ", app_path ," | awk '{print $1}'"),
                               intern = TRUE, timeout = 1))
        if (inherits(disk_now, "try-error") || length(disk_now) == 0) return("Cannot get it")
        return(disk_now)
    }

    getTemp <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        temp_now <- try(system('cat /sys/class/thermal/thermal_zone*/temp', intern = TRUE, timeout = 1))
        if (inherits(temp_now, "try-error") || length(temp_now) == 0) return("Cannot get it")
        temp_type <- try(system('cat /sys/class/thermal/thermal_zone*/type', intern = TRUE, timeout = 1))
        if (inherits(temp_type, "try-error") || length(temp_type) == 0) return("Cannot get it")
        tibble::tibble(
            type = c("average", temp_type),
            temp = c(round(mean(as.numeric(temp_now))/1000, 2), as.numeric(temp_now)/1000))
    }

    tempString <- function(temp_now){
        if (is.character(temp_now)) return(temp_now)
        color <- if (temp_now[1, 2] > 80) "red" else if (temp_now[1, 2] > 60) "orange" else "olive"
        return(list(HTML(paste0(temp_now[1, 2], "&#176;C")), color))
    }

    ## module start ----
    module <- function(input, output, session){
        ns <- session$ns
        observe({
            output$uptime <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "App up time", timeDiffString(), icon = icon("clock"),
                    color = "blue"
                )
            )
            output$disk_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "Current app size", diskString(), icon = icon("save"),
                    color = "blue", width = 3
                )
            )
            invalidateLater(60000)
        })
        # update plot only when users are looking at them, save resources
        observeEvent(shared$topInput[['admin-left_sidebar']], {
            if (shared$topInput[['admin-left_sidebar']] == "admin-info") {
                update_all$resume()
            } else {
                update_all$suspend()
            }
        })
        # plots -----
        ## cpu
        time_init <- as.POSIXlt(Sys.time(), "UTC")
        output$cpu_use_plot <- renderPlotly({
            cpu_init <- getCPU(0.01)
            if(!is.data.frame(cpu_init)) return(plotly::plot_ly(type = "scatter", mode = "a"))
            p1 <- plotly::plot_ly(type = 'scatter', mode = 'lines')
            for(i in seq_len(nrow(cpu_init))) {
                p1 <- p1 %>% plotly::add_trace(x= seq(-30, 0) + time_init, y = 0, name = cpu_init[['V1']][i])
            }
            p1 %>% plotly::layout(
                xaxis = list(title = "UTC Time"),
                yaxis = list(title = "CPU Usage (%)", range = c(0, 100)))
        })
        ## temp
        output$cpu_temp_plot <- renderPlotly({
            cpu_temp <- getTemp()
            if(!is.data.frame(cpu_temp)) return(plotly::plot_ly(type = "scatter", mode = "a"))
            p1 <- plotly::plot_ly(type = 'scatter', mode = 'lines')
            for(i in seq_len(nrow(cpu_temp))) {
                p1 <- p1 %>% plotly::add_trace(x= seq(-30, 0) + time_init, y = 0, name = cpu_temp[['type']][i])
            }
            p1 %>% plotly::layout(
                xaxis = list(title = "UTC Time"),
                yaxis = list(title = HTML("CPU Temperature &#176;C")))
        })
        ## ram
        output$ram_plot <- renderPlotly({
            ram <- getRAM()
            if(!is.numeric(ram)) return(plotly::plot_ly(type = "scatter", mode = "a"))
            plotly::plot_ly(type = 'scatter', mode = 'lines') %>%
                plotly::add_trace(x= seq(-30, 0) + time_init, y = 0, name = "Main", line=list(color='#ff7f0e')) %>%
                plotly::add_trace(x= seq(-30, 0) + time_init, y = 0, name = "Swap", line=list(color='#2ca02c')) %>%
                plotly::add_trace(x= seq(-30, 0) + time_init, y = 0, yaxis = "y2", line=list(color='black'), showlegend = FALSE) %>%
                plotly::layout(
                    xaxis = list(title = "UTC Time"),
                    yaxis = list(title = "RAM Usage (G)", range = c(0, max(ram[2], ram[4])/1024 + 1)),
                    yaxis2 = list(title = "RAM Usage (%)", range = c(0, 100), side = "right", overlaying= 'y'),
                    shapes = list(
                        list(
                            type = "line",
                            y0 = ram[2]/1024, y1 = ram[2]/1024, xref = "paper",
                            x0 = 0, x1 = 1, line = list(color = "#ff7f0e", dash='dot')
                        ),
                        list(
                            type = "line",
                            y0 = ram[4]/1024, y1 = ram[4]/1024, xref = "paper",
                            x0 = 0, x1 = 1, line = list(color = "#2ca02c", dash='dot')
                        )
                    ))
        })
        # update all ----
        update_all <- observe({
            invalidateLater(10000)
            time_now <- Sys.time()
            # cpu
            cpu <- getCPU()
            output$cpu_now <- shinydashboard::renderInfoBox({
                box_text <- cpuString(cpu)
                shinydashboard::infoBox(
                    "CPU usage", box_text[1], icon = icon("microchip"),
                    color = box_text[2], width = 3
                )
            })
            if(is.data.frame(cpu)) {
                plotly::plotlyProxy("cpu_use_plot", session) %>%
                    plotly::plotlyProxyInvoke(
                        "extendTraces",
                        list(
                            x = lapply(rep(time_now, nrow(cpu)), list),
                            y = lapply(cpu[['V2']], list)
                        ),
                        Reduce(append, seq_len(nrow(cpu)), list()),
                        30
                    )
            }
            # temp
            cpu_temp <-  getTemp()
            output$cpu_temp <- shinydashboard::renderInfoBox({
                box_text <- tempString(cpu_temp)
                shinydashboard::infoBox(
                    "CPU temp", box_text[[1]], icon = icon("thermometer-half"),
                    color = box_text[[2]], width = 3
                )
            })
            if (is.data.frame(cpu_temp)) {
                plotly::plotlyProxy("cpu_temp_plot", session) %>%
                    plotly::plotlyProxyInvoke(
                        "extendTraces",
                        list(
                            x = lapply(rep(time_now, nrow(cpu_temp)), list),
                            y = lapply(cpu_temp[['temp']], list)
                        ),
                        Reduce(append, seq_len(nrow(cpu_temp)), list()),
                        30
                    )
            }
            # ram
            ram <- getRAM()
            output$ram_now <- shinydashboard::renderInfoBox({
                box_text <- ramString(ram)
                shinydashboard::infoBox(
                    "RAM main usage", box_text[1], icon = icon("memory"),
                    color = box_text[2], width = 3
                )
            })
            output$swap_now <- shinydashboard::renderInfoBox({
                box_text <- swapString(ram)
                shinydashboard::infoBox(
                    "RAM swap usage", box_text[1], icon = icon("memory"),
                    color = box_text[2], width = 3
                )
            })
            if (is.numeric(ram)){
                plotly::plotlyProxy("ram_plot", session) %>%
                    plotly::plotlyProxyInvoke(
                        "extendTraces",
                        list(
                            x = list(list(time_now), list(time_now), list(time_now)),
                            y = list(list(ram[1]/1024), list(ram[3]/1024), list(0))
                        ),
                        list(1, 2, 3),
                        30
                    )
            }
        }, suspended = TRUE)
    }
    moduleServer(id, module)
}
