% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spsCore.R
\name{spsOptDefaults}
\alias{spsOptDefaults}
\alias{spsOptions}
\title{Print SPS options}
\usage{
spsOptDefaults(app_path = getwd())

spsOptions(app_path = getwd(), show_legend = TRUE)
}
\arguments{
\item{app_path}{path, where is the app directory}

\item{show_legend}{bool, show the color legend?}
}
\value{
cat to console SPS option values


}
\description{
Make sure you have created the app directory and it
has \emph{config/config.yaml} file.

\link{spsOptDefaults} prints out all default and other avaliable values for
each option. \link{spsOptions} print all current set option values.

Note: the \link[spsUtil:spsOption]{spsUtil::spsOption} is used to get or set a \strong{single} option value.
\link{spsOptions} is used to print \strong{all} current option values. If you need to
set all values at once, use the \emph{global.R} file under SPS project root.
}
\examples{
if(interactive()){
    # start a SPS project
    spsInit(open_files = FALSE)
    viewSpsDefaults()
    # change a few options
    options(sps = list(
        mode = "server",
        warning_toast = TRUE,
        login_screen = FALSE,
        login_theme = "vhelix",
        use_crayon = TRUE
    ))
    # view current options
    spsOptions()
}
}
