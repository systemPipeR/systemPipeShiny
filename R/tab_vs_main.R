## UI
vs_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Visualization"),
        renderDesc(id = ns("desc"),
        '
        #### Use Visualization in SPS
        This is the visualization(VS) module. The main purpose is to help
        you **quickly generate some plots**.

        #### Quick start
        Tabs in VS have two categories: data tabs (under "Prepare dataset") and
        plot tabs ("Collection of plots"). You need to prepare some data by
        uploading or using the examples before you can make a plot.

        You can browse all current data tabs in the table below and all plot
        tabs also in the gallery below. Clicking on the table cells or gallery
        images will redirect you to the corresponding tab.

        Normally if you have run a data analysis workflow and want to make some
        plots, find the data tab that matches the type of data you have in hand
        and upload data to that data tab to start.

        Depending on the **preprocess** method you select, different plot options
        will be given.

        If you see any plots in the gallery that you want to make but do not
        know what data you should prepare. Go to that plot tab and read
        instructions there. The plot tab should clearly tell you what type of
        data is required and can be prepared at which data tab.

        Some plots requires more than one dataset, e.g. metadata and raw value
        data. In this case, you need to prepare data set in different data tabs.
        Clicking on "Start/Reload" button on each plot tab can help you to check
        what data set is missing and if the dataset has the right format. Once
        all requirements are met, the rest of the plot tab UI will be displayed
        and it should be ready to render the plot. Plot tabs usually come with
        some plot control UIs that can change the plot looking.

        Clicking on the "Render/Snapshot" button from the **second** time and
        on will take
        a *snapshot* of a plot. You can see captured snapshots on the "Canvas"
        tab and delete/download/upload them from the very top banner of SPS
        dashboard "Snapshots" with a camera icon.

        Follow the [vignette example](https://systempipe.org/systemPipeShiny/articles/systemPipeShiny.html)
        will help you to better understand how snapshot-Canvas work in SPS.
        '),
        spsHr(),
        br(),
        genHrefTable(rows = list(`Data tabs in this project` =  "data"),
                     title = "All SPS data input options"),
        br(),
        genGallery(type = "plot", title = "All current plot options")

    )
}

## server
vs_mainServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

