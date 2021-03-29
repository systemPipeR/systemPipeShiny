## UI
admin_infoUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("App general information"),
        spsHr(),
        br(),
        fluidRow(
            shinydashboard::infoBoxOutput(ns("uptime")),
            shinydashboard::infoBoxOutput(ns("cpu_now")),
            shinydashboard::infoBoxOutput(ns("ram_now")),
            shinydashboard::infoBoxOutput(ns("disk_now"))
        )
    )
}



## server
admin_infoServer <- function(id, shared){

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

    cpuString <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        cpu_now <- try(system("top -bn 2 -d 0.5 | grep '^%Cpu' | tail -n 1 | awk '{print $2+$4+$6\"%\"}'",
                              intern = TRUE, timeout = 1))
        if (inherits(cpu_now, "try-error") || length(cpu_now) == 0) return("Cannot get it")
        return(cpu_now)
    }

    ramString <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        ram_now <- try(system("free -h | grep Mem | awk '{print $3 \"/\" $2}'",
                              intern = TRUE, timeout = 1))
        if (inherits(ram_now, "try-error") || length(ram_now) == 0) return("Cannot get it")
        return(ram_now)
    }

    diskString <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        app_path <- if(isFALSE(spsOption("app_path")))  getwd() else spsOption("app_path")

        disk_now <- try(system(paste0("du -sh ", app_path ," | awk '{print $1}'"),
                               intern = TRUE, timeout = 1))
        if (inherits(disk_now, "try-error") || length(disk_now) == 0) return("Cannot get it")
        return(disk_now)
    }

    module <- function(input, output, session){
        ns <- session$ns
        observe({
            output$uptime <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "App up time", timeDiffString(), icon = icon("clock"),
                    color = "blue"
                )
            )
            invalidateLater(60000)
        })
        observe({
            output$cpu_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "CPU usage", cpuString(), icon = icon("microchip"),
                    color = "blue"
                )
            )
            output$ram_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "RAM usage", ramString(), icon = icon("memory"),
                    color = "blue"
                )
            )
            output$disk_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "Current app size", diskString(), icon = icon("save"),
                    color = "blue"
                )
            )
            invalidateLater(10000)
        })
    }
    moduleServer(id, module)
}
