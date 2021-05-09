
<!-- README.md is generated from README.Rmd. Please edit that file -->

# castarter2 <a href='https://github.com/giocomai/castarter2'><img src='man/figures/hex-castarter2.png' align="right" height="320" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

castarter2 is a more modern, fully-featured, and consistent iteration of
[`castarter`](https://github.com/giocomai/castarter).

It is currently at an early stage of development, and will likely behave
erratically.

## Installation

You can installcastarter2 with:

``` r
remotes::install_github("giocomai/castarter2")
```

## Interactive exploration of textual corpora

Check out `castarter2`’s interactive web interface for exploring
corpora.

``` r
remotes::install_github("giocomai/tifkremlinen")
cas_explorer(corpus = tifkremlinen::kremlin_en,
             default_string = "Putin, Medvedev")
```

# To do

## Getting files

  - make castarter download files in the background
      - with callr
      - possibly, via rstudio jobs

## Time series

  - more options for analysis: peaks, periods, etc.
