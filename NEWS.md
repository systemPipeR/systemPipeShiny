# systemPipeShiny 1.1.10

Changes made from 1.1.0 to 1.1.10
<!--#   YYYY.MM.DD -->

## 2020.11.24

### Workflow module R session

-   Now workflow module R session uses a background child R process, which runs independently to the parent R session which runs shiny.
-   So the shiny will be not blocked while code is running in the background (you can still click other buttons when the child session is busy) -- synchronous and non-blocking. A child indicator is also placed in the UI, updates every second.
-   The UI design of R session is similar to Rstudio. Four panels, source code, console, log (specific to SPR), and plots.
-   Standard out/error and plots are captured in the workflow folder. Users can download them in the bundle on step 5 *Workflow Run.*
-   Plots will be displayed on the plots panel. Now supports plots that opens R `device` (base and ggplot), html widget plots are not supported as this moment.

### General UI

-   A new shiny disconnection popup for SPS. Besides the gray layer on shiny disconnection, a panel will be displayed to users to indicate the problem. Similar to what shows on a shiny server, but more informative and also works locally.

### RNAseq module

-   redesigned the UI and server logic. Plots for DEG analysis and Canvas connections will be added soon.

## 2020.10.30

### General UI

-   Added a "Go Top" button on the right bottom corner, clicking on this button will automatically scroll to the top of the page. This button only shows up when client has \> 50px scroll height.

## 2020.10.30

### Workflow module CWL tab

-   Now the CWL file and CWL input file can be be edited. The edits will be imported to CWL parser every one second. Now this is a very useful place to test or write new CWL scripts.

## 2020.10.27

### Workflow module CWL tab

-   Now this tab has a dynamically rendered dropdown panel which allows users to choose which column for the targets table to map to the variables in CWL input file.

## 2020.10.27

### Workflow module fully functioning

-   Now you can run a full example workflow in SPS by choosing the "Example" option on workflow setup step.

-   Other systemPipeR preconfiged workflows will cause problems because formatting issues that will cause errors in `systemPipeR::runWF` function, not introduced by SPS. Please wait the updates on systemPipeR to fix this. You can still use SPS to prepare files for all workflows. That means, step 1-4 will work, step 5 will give you errors if you choose a workflow which is not "Example".

## 2020.10.22

### Rework on the workflow part

-   All 3 tabs merged into the main tab

-   changed config tab to CWL tab

-   added support for the running wf as a sub tab

-   Now the main tab has 5 subtabs, they are all connected.

-   Better guidelines for users, step-like usage, can't reach other steps if a previous step is not completed.

-   Original snapshot management drop down page changed to running workflow session. This session will lock users to a unique page, they can't interactive other app parts on the page(working directory changed), to prevent app crash due to wd change.

### Other changes

-   A new UI component `spsTimeline` : horizontal timeline UI unit, has status, can be updated on server by `updateSpsTimeline`.

-   A new UI `bsHoverPopover`: enhanced high level function of `bsPlus::HoverPopover`, additional JS used to make the popover work on buttons as well.

-   Fixed some link problems in `renderDesc`. Better links in `renderDesc`, enlarged and spacing animation for links.

-   Change on about page

    -   The news is now rendered on *about* tab in the app

    -   reduced developer content on about page.

    -   changed developer emails to github links.

## 2020.10.09

### Change on visualization

-   RNAseq part is now only in one tab as big module: users upload the targets file and a raw count table, and make different plots in subtabs.

    -   This introduced a lot of dependencies, will decide later if we keep as it is or separate it to *spsBio.*
