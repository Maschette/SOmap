
<!-- README.md is generated from README.Rmd. Please edit that file -->
SOmap
=====

The goal of SOmap is to make publication quality round Southern Ocean maps in polar projections with little effort.

Installation
------------

The development version from [GitHub](https://github.com/Maschette/SOmap) with:

``` r
# install.packages("devtools")
devtools::install_github("Maschette/SOmap")
```

Example
-------

To make a simple map you can use the following function; use ?SOmap to see all the options for modifying layers.

``` r
SOmap::SOmap()
#> Loading required namespace: rgeos
```

<img src="man/figures/README-example-1.png" width="100%" />

    #> [1] "Congratulations, you did a thing!"

There is also SOmanagement() which have management layers for the Southern Ocean and SOleg() which gives custom rounded legends for added map layers

``` r
SOmap::SOmap()
#> [1] "Congratulations, you did a thing!"
SOleg(position="topright",
      col=spirited,
      ticks=6,
      tlabs = c("0","20","40","60","80","100"),
      Trim=-45,
      label="Sea Ice")
plot(ice, col=spirited, add=T,legend=FALSE, alpha=0.95) ## From raadtools
SOmap::SOmanagement(EEZ = T)
```

<img src="man/figures/README-pressure-1.png" width="100%" />

    #> [1] "ooooh pretty!"
