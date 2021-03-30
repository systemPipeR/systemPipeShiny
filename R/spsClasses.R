######################## SPS R6 Classes #######################

# Imports for database handling classes
#' @importFrom R6 R6Class
#' @importFrom RSQLite dbDisconnect dbListTables dbWriteTable dbGetQuery
#' @importFrom RSQLite dbSendStatement dbGetRowsAffected
#' @importFrom RSQLite dbClearResult dbConnect SQLite
#' @importFrom dplyr tribble tbl collect pull
#' @importFrom openssl rsa_keygen encrypt_envelope decrypt_envelope
NULL


#' SPS database functions
#'
#' @description Initiate this container at global level.
#' Methods in this class can help admin to
#' manage general information of SPS. For now it stores some meta data,
#' the encryption key pairs and the account info. You can use this database to store
#' other useful things, like user password hash, IP, browsing info ...
#'
#' A SQLite database by default is created inside `config` directory.
#' If not, you
#' can use `createDb` method to create one. On initiation, this class checks
#' if the default db is there and gives warnings if not.
#'
#' One instance of this class is created by the [spsAccount] super class in
#' *global.R*, normal users don't need to change anything.
#' @export
#' @examples
#' dir.create("config", showWarnings = FALSE)
#' mydb <- spsDb$new()
#' mydb$createDb()
#' mydb$queryValue("sps_meta")
#' mydb$queryInsert("sps_meta", value = "'new1', '1'")
#' mydb$queryValue("sps_meta")
#' mydb$queryInsert("sps_meta", value = c("'new2'", "'2'"))
#' mydb$queryValue("sps_meta")
#' mydb$queryUpdate("sps_meta", value = '234',
#'                  col = "value", WHERE = "info = 'new1'")
#' mydb$queryValue("sps_meta")
#' \dontrun{
#'     library(dplyr)
#'     mydb$queryValueDp(
#'         "sps_meta",
#'         dp_expr="filter(., info %in% c('new1', 'new2') %>% select(2)")
#' }
#' mydb$queryDel("sps_meta", WHERE = "value = '234'")
spsDb <- R6::R6Class("spsDb",
    public = list(
        #' @description initialize a new class object
        initialize = function(){
            spsinfo("Created SPS database method container", verbose = TRUE)
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            fail_msg <- c("Can't find default SPS db. ",
                          "Use `createDb` method",
                          " to create new or be careful ",
                          "to change default db_name ",
                          "when you use other methods")
            con <- private$dbConnect("config/sps.db")

            if(is.null(con)){
                spswarn(fail_msg)
            } else if(!all(c("sps_raw", "sps_meta") %in%
                           RSQLite::dbListTables(con))){
                spsinfo("Connected, but tables missing, seems like a new db.",
                        TRUE)
                spsinfo("Use CreateDb method to create tables", TRUE)
            } else {
                spsinfo("Default SPS-db found and is working")
            }
        },

        #' @description Create a SPS database
        #'
        #' @param db_name database path, you need to
        #' manually create parent directory
        #' if not exists
        createDb = function(db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            if(!dir.exists(dirname(db_name)))
                dir.create(dirname(db_name), recursive = TRUE)
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't create db. Make sure your wd is writeable")
            } else {
                spsinfo("Creating SPS db...", TRUE)
                spsinfo("Db write the meta table")
                sps_meta <- dplyr::tribble(
                    ~info, ~value,
                    "creation_date",
                    as.character(format(Sys.time(), "%Y%m%d%H%M%S")),
                )
                RSQLite::dbWriteTable(con, 'sps_meta',
                                      sps_meta, overwrite = TRUE)

                key <- private$genkey()
                key_encode <- key %>% serialize(NULL)

                spsinfo("Db write the raw table")
                sps_raw <- dplyr::tribble(
                    ~info, ~value,
                    "key", key_encode,
                )
                RSQLite::dbWriteTable(con, 'sps_raw', sps_raw, overwrite = TRUE)
                spsinfo("Key generated and stored in db")

                spsinfo("Db create admin account")
                sps_account <- dplyr::tribble(
                    ~account, ~pass, ~role,
                    "admin", openssl::sha256("admin", key), "admin",
                    "user", openssl::sha256("user", key), "user"
                )
                RSQLite::dbWriteTable(con, 'sps_account', sps_account, overwrite = TRUE)

                msg(c(glue("Done, Db created at '{db_name}'. "),
                      "DO NOT share this file with others or upload to open access domains."),
                    "SPS-DANGER", "red")
                msg(glue("Key md5 {glue_collapse(key$pubkey$fingerprint)}"),
                    "SPS-INFO", "orange")
            }
        },

        #' @description Query database
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param SELECT  SQL select grammar
        #' @param WHERE SQL where grammar
        #' @return query result, usually a dataframe
        queryValue = function(table, SELECT="*",
                              WHERE="1", db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                results <- RSQLite::dbGetQuery(con, glue("
                    SELECT {SELECT}
                    FROM {table}
                    WHERE {WHERE}
                "))
                spsinfo("Query sent")
                return(results)
            }
        },

        #' @description Query database with [dplyr] grammar
        #'
        #' Only supports simple selections, like comparison, %in%, `between()`,
        #' `is.na()`, etc. Advanced selections like wildcard,
        #' using outside dplyr functions like `[stringr::str_detect()]`,
        #'  `[base::grepl()]` are not supported.
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param dp_expr dplyr chained expression, must use '.' in first
        #' component of the chain expression
        #' @return query result, usually a tibble
        queryValueDp = function(table, dp_expr="select(., everything())",
                                 db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                msg("Can't find db.", "error")
            } else {
                results <- dplyr::tbl(con, table) %>%
                    {rlang::eval_tidy(rlang::parse_expr(dp_expr))} %>%
                    dplyr::collect()
                spsinfo("Query sent")
                return(results)
            }
        },

        #' @description update(modify) the value in db
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param value  new value
        #' @param col  which column
        #' @param WHERE SQL where statement, conditions to select rows
        queryUpdate =  function(table, value, col,
                                WHERE="1", db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                res <- RSQLite::dbSendStatement(con, glue(
                    "UPDATE {table} SET {col}={value} WHERE {WHERE}"))
                if({aff_rows <- RSQLite::dbGetRowsAffected(res)} == 0){
                    spsinfo("No row updated", verbose = TRUE)
                } else {
                    spsinfo(glue("Updated {aff_rows} rows"), verbose = TRUE)
                }
                RSQLite::dbClearResult(res)
            }
        },

        #' @description delete value in db
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param WHERE SQL where statement, conditions to select rows
        queryDel =  function(table, WHERE="1", db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                res <- RSQLite::dbSendStatement(con, glue(
                    "DELETE FROM {table} WHERE {WHERE}"))
                if({aff_rows <- RSQLite::dbGetRowsAffected(res)} == 0){
                    spsinfo("No row deleted", verbose = TRUE)
                } else {
                    spsinfo(glue("Deleted {aff_rows} rows"), verbose = TRUE)
                }
                RSQLite::dbClearResult(res)
            }
        },

        #' @description Insert value to db
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param value  new values for the entire row, collect all values from
        #' all columns in a vector.
        queryInsert =  function(table, value, db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                msg("Can't find db.", "error")
            } else {
                value <- glue_collapse(value, sep = ", ")
                res <- RSQLite::dbSendStatement(con, glue(
                    "INSERT INTO {table} VALUES ({value})"))
                if({aff_rows <- RSQLite::dbGetRowsAffected(res)} == 0){
                    spswarn("No row inserted")
                } else {
                    spsinfo(glue("Inerted {aff_rows} rows"), verbose = TRUE)
                }
                RSQLite::dbClearResult(res)
            }
        }
    ),
    private = list(
        dbConnect = function(db_name){
            if(!is.character(db_name) | length(db_name) != 1)
                spserror("Invalid db name")
            con <- tryCatch(
                RSQLite::dbConnect(
                    RSQLite::SQLite(),
                    normalizePath(db_name, mustWork = FALSE)
                ),
                error = function(e){
                    spswarn(e)
                    return(NULL)
                }
            )
            spsinfo("Db connected")
            return(con)
        },
        genkey = function(){
            return(openssl::rsa_keygen())
        }
    )
)

#' SPS encryption functions
#'
#' @description
#' Methods in this class can help admin to encrypt files been output from sps.
#' For now it is only used to encypt and decrypt snapshots.
#' This class requires the SPS database. This class inherits all functions from
#' the [spsDb] class, so there is no need to initiate the `spsDb` container.
#'
#' This class is required to run a SPS app. This class needs to be initialized
#' global level. This has already been written in *global.R* for you.
#' @export
#' @examples
#' dir.create("config", showWarnings = FALSE)
#' spsOption('verbose', TRUE)
#' my_ecpt <- spsEncryption$new()
#' my_ecpt$createDb()
#' # Read carefully before change the key
#' my_ecpt$keyChange()
#' # confirm
#' my_ecpt$keyChange(confirm = TRUE)
#' # imagine a file has one line "test"
#' writeLines(text = "test", con = "test.txt")
#' # encrypt the file
#' my_ecpt$encrypt("test.txt", "test.bin", overwrite = TRUE)
#' # decrypt the file
#' my_ecpt$decrypt("test.bin", "test_decpt.txt", overwrite = TRUE)
#' # check the decrypted file content
#' readLines('test_decpt.txt')
spsEncryption <- R6::R6Class(
    "spsEncryption",
    inherit = spsDb,
    public = list(
        #' @description initialize a new class container
        initialize = function(){
            spsinfo("Created SPS encryption method container", verbose = TRUE)
            spsinfo("This container inherits all functions from spsDb class")
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            fail_msg <- c("Can't create default SPS db. ",
                          "Default name 'config/sps.db'. Use `createDb` method",
                          " to create new or be careful ",
                          "to change default db_name ",
                          "when you use other methods")
            con <- private$dbConnect("config/sps.db")

            if(is.null(con)){
                spswarn(fail_msg)
            } else if(!all(c("sps_raw", "sps_meta", "sps_account") %in%
                           RSQLite::dbListTables(con))){
                spsinfo("Connected, but tables missing, seems like a new db.")
            } else {
                spsinfo("Default SPS-db found and is working", verbose = TRUE)
            }
        },

        #' @description Change encryption key of a SPS project
        #' @param confirm, bool, confirm that you understand the consequence
        #' @param db_name  database path
        keyChange = function(confirm = FALSE, db_name="config/sps.db"){
            stopifnot(is.logical(confirm) && length(confirm) == 1)
            if (!confirm) {
                msg(level = "SPS-DANGER", .other_color = "red", glue("\n
                change this key will result all accounts' password failed to
                authenticate. You have to regenerate all password for all
                accounts. All encrypted file using the old key will fail to
                decrypt. There is NO way to RECOVER the old key, password
                and files. If you wish to continue, recall this function
                with `confirm = TRUE`."))
                return(invisible())
            }
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                old_value <- tryCatch(
                    RSQLite::dbGetQuery(con,
                        "SELECT `value`
                         FROM `sps_raw`
                         WHERE (`info` = 'key')") %>% dplyr::pull() ,
                    error = function(e){
                        spswarn(e)
                        return(NULL)
                    }
                )
                if(length(old_value) == 0) msg("Not a standard sps db", "error")
                key <- private$genkey()
                key_encode <- key %>% serialize(NULL)
                res <- RSQLite::dbSendStatement(con, glue(
                    "UPDATE sps_raw SET value=x'{glue_collapse(key_encode)}'",
                    "WHERE info='key'"))
                if(RSQLite::dbGetRowsAffected(res) != 0){
                    spsinfo("You new key has been generated and saved in db")
                    msg(glue("md5 {glue_collapse(key$pubkey$fingerprint)}"),
                        "SPS-INFO", "orange")
                } else {spserror("Update key failed")}
                RSQLite::dbClearResult(res)
            }
        },

        #' @description Get encryption key from db of a SPS project
        #'
        #' @param db_name  database path
        keyGet = function(db_name="config/sps.db"){
            key <- self$queryValue(table = 'sps_raw', SELECT="value",
                                   WHERE="info='key'", db_name) %>%
                dplyr::pull() %>% unlist() %>% unserialize()
                spsinfo(glue("OpenSSL key found md5",
                             "{glue_collapse(key$pubkey$fingerprint)}"))
            key
        },

        #' @description Encrypt raw data or a file with key from a SPS project
        #'
        #' @param data  raw vector or a file path
        #' @param db_name  database path
        #' @param out_path if provided, encrypted data will be write to a file
        #' @param overwrite if `out_path` file exists, overwrite?
        encrypt = function(data, out_path=NULL,
                           overwrite = FALSE, db_name="config/sps.db"){
            if (!is.null(out_path) & file.exists(out_path) & !overwrite) {
                spserror(glue("File {normalizePath(out_path)} exists"))
            }
            key <- self$keyGet()
            data_ecpt <- openssl::encrypt_envelope(data, key$pubkey)
            class(data_ecpt) <- c(class(data_ecpt), "sps_ecpt")
            spsinfo("Data encrypted")
            if(is.null(out_path)) return(data_ecpt)
            else saveRDS(data_ecpt, out_path)
            spsinfo(glue("File write to {normalizePath(out_path)}"))
            return(invisible(out_path))
        },

        #' @description Decrypt raw data or a file with key from a SPS project
        #'
        #' @param data  raw vector or a file path
        #' @param out_path if provided, encrypted data will be write to a file
        #' @param overwrite if `out_path` file exists, overwrite?
        #' @param db_name  database path
        decrypt = function(data, out_path=NULL,
                           overwrite = FALSE, db_name="config/sps.db"){
            if (!is.null(out_path) & file.exists(out_path) & !overwrite) {
                spserror(glue("File {normalizePath(out_path)} exists"))
            }

            data <- tryCatch({
                if (is.raw(data)) {data}
                else if (all(is.character(data) & length(data) == 1)) {
                    content <- readRDS(normalizePath(data, mustWork = TRUE))
                    spsinfo("File read into memory")
                    content
                }
                else {spserror("data must be raw or a valid file")}
                }, error = function(e){
                    spswarn(e)
                    spserror("Not a standard SPS encrypted object")
                })
            if(!inherits(data, "sps_ecpt"))
                spserror("Not a standard SPS encrypted object")
            key <- self$keyGet()
            data_dcpt <- tryCatch({
                suppressWarnings(
                    openssl::decrypt_envelope(
                        data$data,
                        iv = data$iv,
                        session =data$session,
                        key = key))
                },  error = function(e){
                    spswarn(e)
                    spserror("Cannot decrypt this file")
                })
            spsinfo("Data decrypted")
            if(is.null(out_path)) return(data_dcpt)
            else {
                writeBin(object = data_dcpt, con = out_path)
            }
            spsinfo(glue("File write to {normalizePath(out_path)}"))
            return(invisible(out_path))
        }
    )
)

#' SPS account management functions
#'
#' @description Initiate this container at global level.
#' Methods in this class can help admins to
#' manage accounts in a SPS project.
#'
#' It uses a SQLite database, by default is created inside `config` directory on
#' SPS initialization.
#'
#' You can use it to add/remove users, change user roles, change password,
#' match/verify account, password, role.
#'
#' A default user account "user", with password "user", and a default admin account
#' "admin"  with password "admin" are create for you.
#'
#' For app deployment, PLEASE create your own accounts and DELETE the default ones.
#' @export
#' @examples
#' dir.create("config", showWarnings = FALSE)
#' spsOption("verbose", TRUE)
#' spsOption("use_crayon", TRUE)
#' # create a new container
#' db <- spsAccount$new()
#' db$createDb()
#' # list all accounts
#' db$accList()
#' # add a new user
#' db$accAdd('user2', '!admin12345')
#' # list all accounts include password hash
#' db$accList(include_pass = TRUE)
#' # change password of an account
#' db$accPassChange("user2", "$aaaaaaa")
#' # check if pass changed
#' db$accList(include_pass = TRUE)
#' # change the role of from user to admin
#' db$accRoleChange("user2", "admin")
#' # check role change
#' db$accList()
#' # remove a user
#' db$accRemove("user2")
#' # check accounts again
#' db$accList()
#' # check if username and password matches
#' db$accMatch(acc_name = "user", acc_pass = "user")
#' # wrong pass
#' db$accMatch("user", "user123")
#' # also check if the user has the right role
#' db$accMatch("user", "user", role = "user", match_role = TRUE)
#' db$accMatch("user", "user", role = "admin", match_role = TRUE)
spsAccount <- R6::R6Class(
    "spsaccount",
    inherit = spsEncryption,
    public = list(
        #' @description initialize a new SPS account container
        initialize = function(){
            spsinfo("Created SPS account method container", verbose = TRUE)
            spsinfo("This container inherits all functions from spsEncryption and spsDb class")
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            fail_msg <- c("Can't create default SPS db. ",
                          "Default name 'config/sps.db'. Use `createDb` method",
                          " to create new or be careful ",
                          "to change default db_name ",
                          "when you use other methods")
            con <- private$dbConnect("config/sps.db")

            if(is.null(con)){
                spswarn(fail_msg)
            } else if(!all(c("sps_raw", "sps_meta", "sps_account") %in%
                           RSQLite::dbListTables(con))){
                spsinfo("Connected, but tables missing, seems like a new db.")
            } else {
                spsinfo("Default SPS-db found and is working", verbose = TRUE)
            }
        },

        #' @description list all accounts of the app. Returns a dataframe
        #' @param include_pass bool, include password hash column?
        #' @param db_name  SPS database path
        accList = function(include_pass = FALSE, db_name="config/sps.db") {
            stopifnot(is.logical(include_pass) && length(include_pass) == 1)
            accs <- self$queryValue(table = "sps_account")
            if (!include_pass) accs <- accs[, -2]
            return(accs)
        },

        #' @description add an account to use the app
        #'
        #' @param acc_name  string, account name
        #' @param acc_pass  string, account password
        #' @param db_name  SPS database path
        #' @param role string, what kind role is this user, one of "user", "admin"
        accAdd = function(acc_name, acc_pass, role="user", db_name="config/sps.db") {
            spsinfo("Checking account info")
            private$checkAcc(acc_name)
            private$checkPass(acc_pass)
            role <- match.arg(role, c("user", "admin"))

            private$findAcc(acc_name, stop_on_exist = TRUE)

            key <- quiet(self$keyGet())
            pass_hash <- openssl::sha256(acc_pass, key)
            quiet(self$queryInsert(table = "sps_account", value = c(glue("'{acc_name}'"), glue("'{pass_hash}'"), glue("'{role}'"))))
            spsinfo(glue('Account {acc_name} created.'), TRUE)
        },

        #' @description remove an account
        #'
        #' @param acc_name  string, account name
        #' @param db_name  SPS database path
        accRemove = function(acc_name, db_name="config/sps.db") {
            spsinfo("Checking account info")
            private$findAcc(acc_name)

            self$queryDel(table = "sps_account", WHERE =  glue("account='{acc_name}'"))
            spsinfo(glue('Account {acc_name} removed'), TRUE)
        },

        #' @description change password of an account
        #'
        #' @param acc_name  string, account name
        #' @param acc_pass  string, account new password
        #' @param db_name  SPS database path
        accPassChange = function(acc_name, acc_pass, db_name="config/sps.db") {
            spsinfo("Checking account info")
            private$findAcc(acc_name)
            private$checkPass(acc_pass)

            key <- quiet(self$keyGet())
            new_pass <- openssl::sha256(acc_pass, key)

            db$queryUpdate(
                table = "sps_account", col = "pass",
                WHERE = glue("account='{acc_name}'"),
                value = glue("'{new_pass}'")
                )
            spsinfo(glue('Account {acc_name} password created.'), TRUE)
        },

        #' @description change the role of an account
        #'
        #' @param acc_name  string, account name
        #' @param role  string, one of "user" or "admin"
        #' @param db_name  SPS database path
        accRoleChange = function(acc_name, role, db_name="config/sps.db") {
            spsinfo("Checking account info")
            private$findAcc(acc_name)

            role <- match.arg(role, c("user", "admin"))

            db$queryUpdate(table = "sps_account", col = "role", WHERE = glue("account='{acc_name}'"), value = glue("'{role}'"))
            spsinfo(glue('Account {acc_name} role changed.'), TRUE)
        },

        #' @description Try to see if the account name exists and has the right
        #' password and role type, useful for login authentification.
        #'
        #' @param acc_name  string, account name
        #' @param acc_pass  string, account new password
        #' @param role  string, one of "user" or "admin"
        #' @param match_role bool, also verify the account role type?
        #' @param db_name  SPS database path
        accMatch = function(
            acc_name, acc_pass, role="user",
            match_role = FALSE, db_name="config/sps.db"
        ) {
            spsinfo("Matching account info")
            stopifnot(is.logical(match_role) && length(match_role) == 1)
            stopifnot(is.character(acc_pass) && length(acc_pass) == 1)
            role <- match.arg(role, c("user", "admin"))

            # find acc name
            if(inherits( try(private$checkAcc(acc_name)), "try-error")) return(FALSE)
            acc <- try(private$findAcc(acc_name), silent = TRUE)
            if(inherits(acc, "try-error")) return(FALSE)

            # match pass
            key <- quiet(self$keyGet())
            if (acc[1, 2] != openssl::sha256(acc_pass, key)) return(FALSE)
            if (match_role && acc[1, 3] != role) return(FALSE)
            return(TRUE)
        }
    ),
    private = list(
        findAcc = function(acc_name, stop_on_exist = FALSE) {
            stopifnot(is.character(acc_name) && length(acc_name) == 1)
            acc <- quiet(self$queryValue(table = "sps_account", WHERE = glue("account='{acc_name}'")))
            if (!stop_on_exist) {
                if(nrow(acc) < 1) spserror("Account does not exist")
                if(nrow(acc) > 1) spserror("More than one account matched, this should not happen, contact the admin!")
            } else {
                if(nrow(acc) == 1) spserror("Account exists")
            }
            return(acc)
        },
        checkAcc = function(acc_name) {
            stopifnot(is.character(acc_name) && length(acc_name) == 1)
            if(stringr::str_detect(acc_name, '[^[:alnum:]]'))
                spserror("No specail character or space allowed for account name")
            if(substr(acc_name, 1, 1) %>% str_detect("[0-9]"))
                spserror("First letter for account name cannot be a number")
            return(TRUE)
        },
        checkPass = function(acc_pass) {
            stopifnot(is.character(acc_pass) && length(acc_pass) == 1)
            if(!stringr::str_detect(acc_pass, '[^[:alnum:]]'))
                spserror("At least one special character for password")
            if(nchar(acc_pass) < 8)
                spserror("At least 8 characters for password")
            return(TRUE)
        }
    )
)






























