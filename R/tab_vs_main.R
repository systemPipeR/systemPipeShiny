## UI
vs_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Visualization"),
        renderDesc(id = ns("desc"),
        '
        ### Use custom tabs in SPS
        This is the custom visualization main tab. The main purpose here is to help
        you **do data visualization (VS) by quickly generating some plots**.

        Here are some example tabs you can play with, and more importantly, you
        can build your own tabs by using the [`spsNewTab`{blk}](https://systempipe.org/sps/funcs/sps/reference/spsNewTab.html)
        function. This function
        will give you a template and register the tab to the app. Then you can load
        your tab by pass your new tab ID to the `sps` main function as an argument` vstabs = c("your_new_tab_id")`
        in the *global.R* file.

        #### Templates
        - We have made the template as easy as possible. It is totally okay if you
          know nothing about Shiny. For beginners, all you
          need to do is to create a new tab and add your plotting code. Interaction
          with other part of SPS has all been handled for you.

        - If you know some shiny code, turn `spsNewTab(template = "full")` will give
          you the full app code, so you can make more customization.

        #### Higher level customization
        SPS is built on [Shiny Modules{blk}](https://shiny.rstudio.com/articles/modules.html),
        so after register the new tab to SPS, you can change the content to whatever
        you want, as long as it is a legal Shiny module.

        #### More instructions
        Read more instructions about [SPS tabs{blk}](https://systempipe.org/sps/adv_features/tabs/)
        and your custom new tabs on
        [our website{blk}](https://systempipe.org/sps/adv_features/tabs/#add-a-new-custom-tab)

        '),
        spsHr(),
        br(),
        genGallery(type = "plot", title = "All current custom data visualization tabs")
    )
}

## server
vs_mainServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

