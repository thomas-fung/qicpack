 <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)[![CRAN status](https://www.r-pkg.org/badges/version/QICpack)](https://CRAN.R-project.org/package=QICpack)
  [![Travis build status](https://travis-ci.org/thomas-fung/qicpack.svg?branch=master)](https://travis-ci.org/thomas-fung/qicpack)
  <!-- badges: end -->
  
# qicpack: QIC Package for GEE

The `qicpack` package calculates QIC of [Pan (2001)](https://doi.org/10.1111/j.0006-341X.2001.00120.x) for Generalized Estimating Equations. This is designed to work as an add-on to `geepack` of [Højsgaard, Halekoh & Yan (2016)](https://cran.r-project.org/web/packages/geepack/index.html). 

## Installation

### Stable release on CRAN

The `qicpack` package is still in active development and so it is not available on [CRAN](https://cran.r-project.org/package=qicpack) yet. 

### Development version on Github

You can use the `devtools` package to install the development version of `qicpack` from [GitHub](https://github.com/thomas-fung/qicpack):

```s
# install.packages("devtools")
devtools::install_github("thomas-fung/qicpack")
library(qicpack)
```

## Usage

A reference manual is available at [thomas-fung.github.io/qicpack](https://thomas-fung.github.io/qicpack/)

## Citation

If you use this package to analyse your data, please use the following citation:

From R you can use:

```s
citation("qicpack")
toBibtex(citation("qicpack"))
```