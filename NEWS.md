## systemPipeShiny Updates

<!--#   YYYY.MM.DD -->

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
