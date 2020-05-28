[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.budget?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-budget/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.budget.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.budget)
[![codecov](https://codecov.io/github/KWB-R/kwb.budget/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.budget)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.budget)]()

# kwb.budget

R package useful during project development (e.g.
H2020 calls with many project partners). By using a budget EXCEL
template file, data received from the different partners can be
aggregated and analysed.)

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.budget' from GitHub
remotes::install_github("KWB-R/kwb.budget")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.budget](https://kwb-r.github.io/kwb.budget)

Development: [https://kwb-r.github.io/kwb.budget/dev](https://kwb-r.github.io/kwb.budget/dev)
