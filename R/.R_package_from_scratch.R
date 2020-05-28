### How to build an R package from scratch

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Hauke Sonnenberg",
               orcid = "0000-0001-9134-2871",
               url = "https://github.com/hsonne")

# author <- list(name = "Michael Rustler",
#                orcid = "0000-0003-0647-7726",
#                url = "https://mrustl.de")

pkg <- list(name = "kwb.budget",
            title = "R Package For Aggregating and Analysing Budget Excel Files from Project Partners",
            desc  = paste("R package useful during project development (e.g. H2020 ",
                          "calls with many project partners). By using a budget",
                          "EXCEL template file, data received from the different",
                          "partners can be aggregated and analysed.)"))

kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("Tutorial")

### R functions


kwb.pkgbuild::use_autopkgdown("kwb.budget")
