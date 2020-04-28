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

tabble_workflow <- data.frame(
    Workflow = c(
        ""
    ),
    hrefs = c(
        ""
    )
    
)
## UI
vs_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        HTML('
        <h1 style="color: #5e9ca0;">Visualization&nbsp;</h1>
        <p><strong>This is the visualization module. The main purpose is to help 
        you quickly generate some plots.&nbsp;</strong></p>
        <p><strong>There are <em>two</em> ways you can use this tab:</strong></p>
        <ol>
        <li>If you know what workflow you have run and have some results but 
        don&#39;t know what you can plot from these results, browse the table 
        below by different workflows, where you can see what data types there 
        are in each workflow. By clicking the data type in the table, you will 
        jump to the corresponding data preparation tab and you will see what 
        plots you can make.</li>
        <li>If you already know what plots you want, browse and click 
        the gallery below and you will jump to the plotting tab directly. You 
        will see what input data type this plot is accepting. By clicking your 
        desired data type, you will go to a data prep tab and once you have 
        prepared the required data, you will be guided back to this plotting tab.
        </li>
        </ol>
             '),
        br(),
        TableHref(item_titles = c("workflow 1", "workflow 2", "workflow 3"),
                  item_labels = list(c("tab 1"), c("tab 3", "tab 4") ,c("tab 5", "tab 6", "tab 7")),
                  item_hrefs = list(c("https://www.google.com/"), c("", ""), c("", "", "")),
        ),
        br(),
        gallery(title = "Gallery of Plots", texts = gallery_plot$texts, hrefs = gallery_plot$hrefs, images = gallery_plot$images, image_frame_size = 3)
        
    )
}

## server
vs_mainServer <- function(input, output, session, shared){

}
    
