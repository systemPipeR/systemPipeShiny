



parseNote <- function(url = spsOption('note_url')){
    # load
    if (!is.character(url) && length(url) != 1){
        spswarn("Notification url needs to be a single string")
        return(NULL)
    }
    if (stringr::str_starts(url, "http")){
        spswarn("Notification url needs to start with 'http(s)'")
        return(NULL)
    }
    if(!spsUtil::checkUrl(url, 2)) {
        spswarn(glue("Cannot reach notification url: {url}"))
        return(NULL)
    }
    notes <- shinyCatch(yaml::read_yaml(url))
    if(is.null(notes)) return(NULL)
    # notes <- yaml::read_yaml("../inst/remote_resource/notifications.yaml")
    mapply(function(note, index){
        if(!.checkNoteExpire(note[['expire']])) return(NULL)
        if(!.checkNotePkg(note[['type']], note[['pkg_name']], note[['version']])) return(NULL)
        if(!emptyIsFalse(note[['title']])) return(NULL)

        msg_icon <- if(emptyIsFalse(note[['icon']])) note[['icon']] else "info-circle"
        msg_body <- if(emptyIsFalse(note[['message']])) note[['message']] else "no details"

        list(
            item = shinydashboardPlus::notificationItem(
                icon = icon(msg_icon), status = .checkStatus(note[['status']]),
                text = note[['title']]
            ) %>%  bsplus::bs_attach_modal(id_modal = paste0("sps-top-note", index)),
            modal = bsplus::bs_modal(
                id = paste0("sps-top-note", index),
                title = note[['title']],
                body = markdown(msg_body),
                size = "large"
            )
        )
    }, note = notes, index = seq_along(notes), SIMPLIFY = FALSE) %>%
        {.[!unlist(lapply(., is.null))]} %>%
        {list(
            items = lapply(., function(i){i[['item']]}),
            modals = lapply(., function(i){i[['modal']]})
        )}
}

.checkNoteExpire <- function(date_str){
    if(!emptyIsFalse(date_str)) return(FALSE)
    note_date <-try(as.Date(date_str), silent = TRUE)
    if (!inherits(note_date, "Date")) return(FALSE)
    if (Sys.Date() > note_date) return(FALSE)
    TRUE
}

.checkNotePkg <- function(type, pkg_name, pkg_verion){
    if(!emptyIsFalse(type)) return(FALSE)
    if(type != "package") return(TRUE)
    if(!emptyIsFalse(pkg_name) || !emptyIsFalse(pkg_verion))  return(FALSE)
    local_verion <- try(utils::packageVersion(pkg_name), silent = TRUE)
    if (inherits(local_verion, "try-error")) return(FALSE)
    remote_version <-try(package_version(pkg_verion), silent = TRUE)
    if (inherits(remote_version, "try-error")) return(FALSE)
    if (local_verion < remote_version) TRUE else FALSE
}

.checkStatus <- function(status){
    if(!emptyIsFalse(status)) return("primary")
    if(!status %in% c('primary', 'success', 'info', 'warning', 'danger')) return("primary")
    status
}
