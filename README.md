# contingencytables

An R package for the _Statistical Analysis of Contingency Tables_ (SACT) book by Fagerland et. al.. More info on the book on [its home page](https://contingencytables.com/) and in the [References](#references) section of this document.

# Installation

## Stable version

The latest stable version of this package is available on [CRAN](https://cran.r-project.org/package=contingencytables) and can be installed by running

```r
install.packages("contingencytables")
```

from an R interactive session.

## Development version

The development version of this package can be installed by running the following command in R:

```r
remotes::install_github("ocbe-uio/contingencytables", ref="develop")
```

If the command above does not work, make sure you have the remotes package installed (e.g. by running `install.packages("remotes")` in R).

# Usage

After installing, you must load the package by running `library("contingencytables")` in R. You can read more about the package and its functions by running `library(help="contingencytables")` or by consulting the SACT book.

# References

To cite this package, install it and run `citation("contingencytables")`. The output should give you proper citation instructions in APA-like and BibTeX formats for the package version you have installed.

Proper citation of the SACT book can be found [here](https://contingencytables.com/how-to-cite).

# License

This software is licensed under the GPL-3. For details on how to use it, please read the [LICENSE](LICENSE) file or its legalese-free summary [here](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)).

# Contributing and getting help

This software is open source and contributions are welcome! If you have any trouble installing or using the package, if you find a bug or would like to submit a suggestion, please let us know by opening a new issue [here](https://github.com/ocbe-uio/contingencytables/issues).

# Nerd badges

[![DOI](https://zenodo.org/badge/293482399.svg)](https://zenodo.org/badge/latestdoi/293482399)
[![](https://www.r-pkg.org/badges/version/contingencytables)](https://cran.r-project.org/package=contingencytables)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build status](https://github.com/ocbe-uio/contingencytables/workflows/R-CMD-check/badge.svg)](https://github.com/ocbe-uio/contingencytables/actions)
[![](https://img.shields.io/github/last-commit/ocbe-uio/contingencytables.svg)](https://github.com/ocbe-uio/contingencytables/commits/develop)
[![](https://img.shields.io/github/languages/code-size/ocbe-uio/contingencytables.svg)](https://github.com/ocbe-uio/contingencytables)
[![](https://codecov.io/gh/ocbe-uio/contingencytables/branch/develop/graph/badge.svg)](https://codecov.io/gh/ocbe-uio/contingencytables)
[![CodeFactor](https://www.codefactor.io/repository/github/ocbe-uio/contingencytables/badge)](https://www.codefactor.io/repository/github/ocbe-uio/contingencytables)
