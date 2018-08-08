# Steps/Commands to run before a release -----------------------------

## Check if version is appropriate
# http://shiny.andyteucher.ca/shinyapps/rver-deps/

## Internal data files
source("data-raw/create_data.R")

## Checks
devtools::check()     # Local
devtools::check_win_release() # Win builder
devtools::check_win_devel()
devtools::check_win_oldrelease()
devtools::check_rhub() # windows/linux/macos

devtools::check_rhub(platforms = "windows-x86_64-devel")

## Run in console
system("cd ..; R CMD build cavityuse")
system(paste0("cd ..; R CMD check cavityuse_",
              packageVersion("cavityuse"),
              ".tar.gz --as-cran"))

## Update codemeta
codemetar::write_codemeta()

## Documentation
# Update NEWS
# Update cran-comments

# Check spelling
dict <- hunspell::dictionary('en_CA')
spelling::spell_check_package(use_wordlist = TRUE)

## Push to github
## Check travis / appveyor

## Update website
## Push to github

## Actually release it (SEND TO CRAN!)
devtools::release()

## Once it is released (Accepted by CRAN) create signed release on github
#system("git tag -s v 0.2.7 -m 'v0.2.7'")
#system("git push --tags")
