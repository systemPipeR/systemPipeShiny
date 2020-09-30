#########################################
## If you use some bioconductor package:
########################################

# options(repos = BiocManager::repositories())

########################
## Set up your account:
########################

## Replace the following arguments:
## - name: Name of account to save or remove
## - token: User token for the account
## - secret: User secret for the account

# setAccountInfo(name="username", token="token", secret="secret")

##########
## Deploy:
##########

# getOption("repos")
# rsconnect::deployApp(
#     appName = "systemPipeShiny",
#     account = "acc_name",
#     lint = TRUE
#     )
