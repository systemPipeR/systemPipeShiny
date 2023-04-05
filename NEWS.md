# systemPipeShiny 1.9.01

## Major Change

-   Add video tutorials to main page and all modules.

## Minor Change

-   Replace the major icons on welcome page, now it has 3 circles instead of 3, reflecting the 3 major functionalities of SPS, workflow, visualization, and canvas.
-   Changed some namings in workflow creation options
    -   empty: Start from scratch
    -   existing: upload custom workflows

## Bug Fix

-   Fix FontAwesome 6.0 introduced name changes

    -   One would also need to install develop version of `spsComps` and `drawer`.

-   Fix not working tab link on welcome page.

-   Fix empty icon problems.

# systemPipeShiny 1.7.03

## General:

-   bump up the version requirement for SPR to 2.2.0, SPRdata to 2.0.0, spsComps to 0.3.2, drawer to 0.2.0.

## Minor change:

-   Since `xl` modal is not supported in bs3, change all modal in WF module to `l` size.
-   Fixed some links
-   add BiocView keywords to description.

## Bug fix:

-   As the new version of SPR, when adding a Linewise step R code, if the code is stored inside a variable, this is not supported. However, SPS has to capture user input code as variable. A workaround is used for now to fix this problem, waiting SPR to support a better way of code in variable or quoted code.

# systemPipeShiny 1.5.10

## Major Change

-   Redesign of the welcome page. Old content is moved to `about`. Now the welcome page is more clear.

-   Adapt SPR to the 2.1.x version.

    -   Add more instructing images to the workflow module.
    -   A warning message is added if `spsOption("demo", TRUE)`, to let people know some workflow templates will fail if they use the demo server to run jobs.

## Minor Change

-   Fix some text typo, links.
-   Add more figures as instructions in different modules/tabs.
-   Text/links fixed in workflow module.

# systemPipeShiny 1.3.15

## New Feature

-   In workflow module, workflow designer (step 3), added two new parameter when creating a new step, `mandatory` and `place of execution`. These are new features added in systemPipeR 1.27.27.

## Minor Change

-   Bump version requirements of `spsComps`, `systemPipeR`, `systemPipeRdata`

-   In global.R, now use `spsOption` with the `.list` argument to set up options instead of the base `options` function.

-   Replace `includeMardown()` by `markdown(readLines())` so we don't need additional {markdown} package as dependency.

## Bug Fix

-   Fix links and image urls that were not working or changed.

# systemPipeShiny 1.3.10

## New Feature

-   Add code display buttons to most plots that will show code to reproduce the plot.

-   Add two args `buttonType` and `placeholder` to `dynamicFile`, now users can specify what bootstrap color the button is and use `placeholder` to specify initial text on the upload bar.

    -   Enhanced the original shiny `fileInput`, now users can also specify icon and button bootstrap colors for "server" mode in `dynamicFile`.

## Major Change

-   Redesign of a few steps in Workflow module. The new version of {systemPipeR} fundamentally changed how the workflow will be run. To sync to this new version, WF module has to been redesigned. Major change happens on workflow step selection. This requires users to install systemPipeR \> 1.27.10

    -   New methods to initiate the WF project

    -   New workflow plot

    -   New step selection mechanism

    -   New step editing functionalities

## Minor Change

-   For RNAseq module, the dendrogram plot library changed from {ggtree} package to {ape}. {ggtree} is not very compatible with Shiny under current version. Plot cannot be created, always error, but no error outside Shiny. An issue has submitted to Shiny on Github. We may switch back to ggtree when this is fixed.
-   Small UI optimization for RNAseq module.
-   Fixed some typo in different tabs.

## Bug Fix

-   [#85](https://github.com/systemPipeR/systemPipeShiny/pull/85) fix `dynamicFile` `icon` not working

    -   Also add some icon validation code

-   Fix the admin server tabs get loaded twice. Added a flag to prevent this from happening.

# systemPipeShiny 1.3.0

-   Update version number to 1.3.0 per Bioconductor regulation.

# systemPipeShiny 1.1.40

## Major change

-   Add `is_demo` option: only affect workflow module right now. Lock users inside a temp folder when using the WF module and give users a new temp folder every time they refresh. This will prevent directory exist problem if many users are using a same deploy instance.

-   Add `welcome_guide` option: whether to enable the welcome guide which highlights the guide dropdown menu.

-   Rewrite welcome tab with a gallery to show all SPS features.

-   `loadDF`, `dynamicFile` and `dynamicFileServer` added back to this mainframe work package instead of `spsComps`, because these dependencies have already been using in `SPS`. Leave these functions in `spsComps` will introduce extra dependencies, and these functions are not too frequently used outside the framework.

## Minor change

-   Option `warning_toast` now also checks if you are on "local" mode.

-   Deleted some unwanted entries in reference generating `yaml` file.

-   Fix some typos.

-   More informative error message when the config file cannot be found for `spsOptions`

-   Add some `.onLoad` methods so users can use the `spsOption` to get default values on package load.

-   Updated `essquise` functions

-   Add more guides.

-   Removed the scroll to top button by `shinyDashboardPlus`, we have our own "go top" button.

-   Add assertions to `spsInit`.

-   Add some screenshots to readme.

## Bug fix

-   Fix a bug when that loads the server twice
-   Fix some default option values
-   Fix a bug on `addResourcePath` when the working directory and app directory is not the same.
-   Fix links not working caused by website change
-   Fix code in `spsInit` overwrite all current SPS options.
-   Fix errors on admin page when server stats cannot be found, better text and warning messages
-   Fix new version of `essquise` introduced errors
-   Fix a warning in `vroom` due to the column type problem

# systemPipeShiny 1.1.35

## Major change

-   Login feature added:

    -   Users can choose whether to enable the login or not in `global.R`SPS options.

    -   There are also the login loading screen feature which can be turned on and off.

    -   There are 3 different login loading screens right now and users can interact with them.

-   Website updated. <https://systempipe.org/sps>

-   Updates on the admin panel:

    -   App information: added live charts for CPU, temperature, and RAM

    -   User control: admins now can add/delete/change users directly from this tab, instead of only from command line.

## Minor change

-   Add`target="_blank"` to all external links in the app, so when they are clicked, it will open in a new tab.

## Bug fix

-   FIx bugs due to login page caused server not loading

-   Add 1s delay in javascript after login to resize the page so the dashboard can be displayed normal.

-   Fix a table rendering bug in workflow cwl step markdown text.

# systemPipeShiny 1.1.30

## Major change

-   new `spsAccount` class. This class is associated with login management , which allows users to create/delete user/admin accounts, change password, change roles.

-   Deprecated the `spsPlotContainer` class since we rewrite the Canvas feature and move to a separate package {drawer}.

-   New `spsCoreTabReplace`, which allows users to overwrite the default core tabs.

-   A lot more SPS options.

    -   Users can now choose whether to load or not load certain tabs on start, even for default core tabs. Combining the `spsCoreTabReplace` function, now users can customize everything of the original app.

    -   Users can change the app title, and logo image.

-   Admin panel added to app. Users now can visit the admin panel by adding "`?user_definded_string`" to the end of the url. Default is "`admin`". Login with an admin account is required. Users can use the `spsAccount` class to add/change an admin account before starting the app.

    -   App information: a tab displays current SPS app server information, like CPU, RAM, size, etc.

    -   User control: a tab to see account information of current SPS app.

-   Changed the way to install modules. Default modules, workflow, RNAseq and quick ggplot dependency packages are not installed by default, unless you use `dependency = TRUE` in installation command. It means all these dependencies are moved from `Imports` to the `Suggests` class. This helps to save quite some time on SPS package installation. Users install these packages based on their needs. When users loads these modules but depend packages are not fully installed, app will not crash, instead, a warning message with install instructions will be displayed on both console and app UI.

-   Based on the module installation change, module loading methods are also changed. Module server functions are only called if users set the option to load them. In previous versions, the server functions are still loaded, just hide the unloaded module UI. This saves a lot of time on app starting time, roughly from \> 10s to \< 3s if none of the default modules are loaded.

## Bug fix

-   update all links to our new website: <https://systempipe.org/sps>

-   Fix some bugs in the guide system

# systemPipeShiny 1.1.20

## Major change

-   3 default modules complete: `workflow`, `RNAseq`,`quick ggplot`. Details of these modules updated in our website: [\<https://systempipe.org/sps\>](https://systempipe.org/sps){.uri}.

-   Separation of SPS smaller functions into 3 different packages. We hope these packages can help people in their own Shiny app, or other R projects.

    -   `{spsComps}`: SPS components, all new Shiny custom components and utility functions that are used in Shiny server side.

    -   `{drawer}`: the redesign of Canvas, purely front-end image editing tool.

    -   `{spsUtil}`: SPS utilities, general useful utility functions that can be used with/without Shiny.

-   Redesigned the new tab feature. Now users use `spsNewTab` function to create their new custom visualization tab. The old `newSpsTab` function is deprecated. Easier syntax and templates are used. By default it will use the "*simple*" template which wraps 90% of the shiny code from users so they can focus on the plotting code. There is also the "*full*" template which expose all the Shiny code to users.

    -   New `spsEzUI` and `spsEzServer` functions are used in the "simple" template to wrap complex Shiny code.

-   New `spsOptDefaults`, which prints out all the default SPS options and current values of these options on console.

-   New notification system. Developers can write some notifications which stores in a remote location and when app starts, it will try to download and parse this file to notifications messages to broadcast to users. This way, developers can send messages to users often without re-deploy the app. The notification will appear on the top right corner.

-   The interactive guide is back. After a few versions of tests, we added the guide system back. This time, developers can customize their own guides. A `guide_content.R` file is created when a SPS project initialize. It is stored in `R` of folder relate to the project root. The guide will also be displayed on the app top right corner.

## Minor change

-   updated all unit test to testthat v3 format.

## Bug fix

-   fix bugs due to shiny updates to 1.6.0

-   Fix all bugs caused by `{shinydashboardPlus}` v2.0 updates.

# systemPipeShiny 1.1.10

Changes made from 1.1.0 to 1.1.05 <!--#   YYYY.MM.DD -->

### Workflow module R session

-   Now workflow module R session uses a background child R process, which runs independently to the parent R session which runs shiny.
-   So the shiny will be not blocked while code is running in the background (you can still click other buttons when the child session is busy) -- synchronous and non-blocking. A child indicator is also placed in the UI, updates every second.
-   The UI design of R session is similar to Rstudio. Four panels, source code, console, log (specific to SPR), and plots.
-   Standard out/error and plots are captured in the workflow folder. Users can download them in the bundle on step 5 *Workflow Run.*
-   Plots will be displayed on the plots panel. Now supports plots that opens R `device` (base and ggplot), html widget plots are not supported as this moment.
-   A new shiny disconnection popup for SPS. Besides the gray layer on shiny disconnection, a panel will be displayed to users to indicate the problem. Similar to what shows on a shiny server, but more informative and also works locally.
-   Results of this session can be downloaded by closing the session and go back to *step 5* of workflow module and there is a button to download all in a zipped file.

### RNAseq module

-   redesigned the UI and server logic. Plots for DEG analysis and Canvas connections.
-   `{SummarizedExperiment}` supports. Now it returns `SummarizedExperiment` objects to global environment once the normalization or DEG calculation is done.

### General UI

-   Added a "Go Top" button on the right bottom corner, clicking on this button will automatically scroll to the top of the page. This button only shows up when client has \> 50px scroll height.

### Workflow module CWL tab

-   Now the CWL file and CWL input file can be be edited. The edits will be imported to CWL parser every one second. Now this is a very useful place to test or write new CWL scripts.
-   Now this tab has a dynamically rendered dropdown panel which allows users to choose which column for the targets table to map to the variables in CWL input file.

### Workflow module fully functioning

-   Now you can run a full example workflow in SPS by choosing the "Example" option on workflow setup step.

-   Other systemPipeR preconfiged workflows will cause problems because formatting issues that will cause errors in `systemPipeR::runWF` function, not introduced by SPS. Please wait the updates on systemPipeR to fix this. You can still use SPS to prepare files for all workflows. That means, step 1-4 will work, step 5 will give you errors if you choose a workflow which is not "Example".

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

### Change on visualization

-   RNAseq part is now only in one tab as big module: users upload the targets file and a raw count table, and make different plots in subtabs.

    -   This introduced a lot of dependencies, will decide later if we keep as it is or separate it to *spsBio.*
