## UI
vs_listUI <- function(id){
    ns <- NS(id)
    tagList(
        p("DNASeq: Plot1, Plot2, Plot3, ..."),
        p("RNASeq: Plot1, Plot2, Plot3, ..."),
        p("RiboSeq: Plot1, Plot2, Plot3, ..."),
        p("methylSeq: Plot1, Plot2, Plot3, ...")
        )
}

## server
vs_listServer <- function(input, output, session, shared){
    
}
