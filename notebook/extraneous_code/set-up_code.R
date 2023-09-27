usethis::create_package(here::here())
roxygen2::roxygenise()
dir.create("notebook")
dir.create("notebook/ext_images")
dir.create("notebook/extraneous_code")
dir.create("notebook/prereg")
dir.create("data")
usethis::use_gpl3_license()

# add data to build ignore
usethis::use_build_ignore("data")
usethis::use_git_ignore("data")

targets::use_targets()

# below not run -----------------------------------------------------------

# get osfr to have means to adding data and results to OSF repo
# remotes::install_github("ropensci/osfr")

library(osfr)

# add OSF_PAT=[OSF PAT KEY HERE FROM https://osf.io/settings/tokens]
usethis::edit_r_environ()

# check access is gained
osfProject <- osf_retrieve_node("https://osf.io/5dgs9")
osfProject

cffr::cff_create()
cffr::cff_gha_update()
