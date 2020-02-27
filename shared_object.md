# Objects saved in `shared`

"Shared" is first defined in the `server.R` as an object to hold data that can
be passed around modules

- wf_flags: bool values to indicate required files status for the workflow
    - targets_ready, wf_ready, wf_conf_ready: target file, workflow Rmd file, config yaml file
- targets: targets file
    - df: dataframe which will be used to display from top push bar
    - file: the temp path of edited targets. When `add to task` is clicked and 
    check passed, this file will be write to temp with **target header**
- count: count table
    - df, file: same as targets
- config: configuration yaml file
    - file: same as targets
