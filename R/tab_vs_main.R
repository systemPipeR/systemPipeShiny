## UI
vs_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        h2("VS main"),
        p("Instructions on how to use the vs utilities."),
        h4("For different workflows, you can choose data types to work with plotting"),
        p("RNAseq: "),
        a(href = "#shiny-tab-df_raw",  "Raw Count"),
        a(href = "",  "df2"),
        a(href = "",  "df3"),
        br(),
        p("DNAAseq: "),
        a(href = "#shiny-tab-df_raw",  "Raw Variant"),
        a(href = "",  "df2"),
        a(href = "",  "df3"),
        gallery(texts = texts, hrefs = hrefs, images = images, image_size = 3)
    )
}

## server
vs_mainServer <- function(input, output, session, shared){

}
