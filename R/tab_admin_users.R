## UI
admin_usersUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("App account management"),
        spsHr(),
        br(),
        h3("Current accounts"),
        DT::DTOutput(ns("acc_table"))
    )
}

## server
admin_usersServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        db <- quiet(spsAccount$new())

        output$acc_table <- DT::renderDT({
            DT::datatable(db$accList())
        })
    }
    moduleServer(id, module)
}
