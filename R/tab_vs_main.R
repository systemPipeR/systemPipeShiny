## UI
vs_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Visualization"),
        HTML('
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
        genHrefTable(rows = list(wf1 = c("df_targets", "df_count"), wf2 =  "data")),
        br(),
        genGallery(type = "plot")

    )
}

## server
vs_mainServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

