#########################################
## If you use some bioconductor package:
########################################
# options(repos = BiocManager::repositories())

## if you use develop version of bioconductor packages, do following:
## if current release is 3.12 and you want use 3.13 devel
# repos <- BiocManager::repositories()
# devel <- "3.13"
# repos[length(repos) + 1] <- paste0("https://bioconductor.org/packages/", devel, "/bioc")
# names(repos)[length(repos)] <- "BioC"
# options(repos = repos)
# getOption("repos")

########################
## Set up your account:
########################

## Replace the following arguments:
## you can find them on shinyapps.io when you create an account
## - username: Name of account to save or remove
## - token: User token for the account
## - secret: User secret for the account

# setAccountInfo(name="username", token="token", secret="secret")

##########
## Deploy:
##########

# getOption("repos")
# rsconnect::deployApp(
#     appName = "systemPipeShiny",
#     account = "username",
#     lint = TRUE
#     )
