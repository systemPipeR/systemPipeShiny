## UI
degUI <- function(id){
  ns <- NS(id)
  tagList(
    h2("Differentially Expressed Genes Analysis"),
    fluidRow(
      boxPlus(title = "Specifications", width = 12, closable = F,
              numericInput(ns("Fold"), "Fold:", value = 2), 
              numericInput(ns("FDR"), "FDR:", value = 10),
              numericInput(ns("cmpset"), "Comparison set:", value = 2), ##1 or 2
              checkboxInput(ns("DESeq2"), "DESeq2"),
              checkboxInput(ns("edgeR"), "edgeR"))
              
    ),
    fluidRow(title = "Plot Output",
             uiOutput(ns("edgeR")),
             uiOutput(ns("DESeq2"))
    )
  )
}

## server
degServer <- function(input, output, session, shared){
  ns <- session$ns
  observe({
    if (!is.null(shared$count_list)){ 
      ## edgeR
        output$edgeR <- renderUI({
          if (input$edgeR == T) {
            output$edgeR_out <- renderPlotly({
              deg_edgeR(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = input$cmpset)
            })
          } else { return(NULL) } 
          boxPlus(plotlyOutput(ns("edgeR_out")))
        })
      # } else if (all(!input$which_ct, !is.null(input$count_targets), !is.null(input$count_input))) {
      #     ct_list <- load_count(targets_file = input$count_targets$datapath, count_table = input$count_input$datapath)
      #     output$edgeR <- renderUI({
      #       if (input$edgeR == T) {
      #         output$edgeR_out <- renderPlotly({
      #         deg_edgeR(countDF = ct_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = ct_list$targets, cmp = ct_list$cmp, cmpset = input$cmpset)
      #         })
      #       } else { return(NULL) } 
      #       boxPlus(plotlyOutput(ns("edgeR_out")))
      #       })
      ## DESeq2
      output$DESeq2 <- renderUI({
        if (input$DESeq2 == T) {
          output$DESeq2_out <- renderPlotly({
            deg_deseq2(targets_file = input$count_targets$datapath, count_table = input$count_table$datapath,
                       Fold = input$Fold, FDR = input$FDR)
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("DESeq2_out")))
      })
  } else {return(NULL)}
  })  
  }
  
 

    
    



