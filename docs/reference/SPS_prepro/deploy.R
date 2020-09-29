# If you use some bioconductor package:

# options(repos = BiocManager::repositories())

## Set up your account:

# setAccountInfo("user", "token", "secret")

## Deploy:

# getOption("repos")
# rsconnect::deployApp(
#     appName = "systemPipeShiny",
#     account = "acc_name",
#     lint = TRUE
#     )
