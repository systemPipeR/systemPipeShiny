gallery_plot <- data.frame(
    texts = c(
        "Bar Plot",
        "PCA Plot",
        "Point Plot",
        "T-SNE Plot"
    ),
    hrefs = c(
        "",
        "#shiny-tab-plot_pca",
        "",
        ""
    ),
    images = c(
        "plot_list/plot_bar.png",
        "plot_list/plot_pca.png",
        "plot_list/plot_point.png",
        "plot_list/plot_tsne.jpg"
    )
)


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
        gallery(title = "Gallery of Plots", texts = gallery_plot$texts, hrefs = gallery_plot$hrefs, images = gallery_plot$images, image_size = 4)
    )
}

## server
vs_mainServer <- function(input, output, session, shared){

}
    
