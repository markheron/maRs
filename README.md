<!-- README.md is generated from README.Rmd. Please edit that file -->

maRs: Mark's awesome R scripts
------------------------------

The R package **maRs** provides several awesome helper functions.

### Summary

This package provides some useful functions that I frequently used:

-   quick summary of large objects in memory
-   ruler style axis for plots
-   nicer (and more convinient) heatmap
-   smearing and running mean functions for fast smoothing

### Installation

``` r
devtools::install_bitbucket(repo="markheron/maRs", auth_user="user_name", password="your_password", keep_source=TRUE)
```

### Usage

``` r
library(maRs)
large_object <- rep(0,10^6)
ram_objects_summary()
#> large_object     7.6 Mb
#> 
#> --------------
#> Other    0 bytes
#> --------------
#> Total    7.6 Mb
```

### License

The **maRs** package is licensed under the GPLv3 (<http://www.gnu.org/licenses/gpl.html>).
