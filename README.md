# ggvis

[![Build Status](https://travis-ci.org/rstudio/ggvis.png?branch=master)](https://travis-ci.org/rstudio/ggvis)

The goal of ggvis is to make it easy to describe interactive web graphics in 
R. It fuses the ideas of [ggplot2](http://github.com/hadley/ggplot2) and 
[shiny](http://github.com/rstudio/shiny), rendering graphics on the web with
[vega](https://github.com/trifacta/vega) (so can draw on the canvas or using
svg). It is less flexible than raw d3 or vega, but much more succinct and
is tailored to the needs of exploratory data analysis.

If you find a clear bug, please file a minimal reproducible example at 
http://github.com/rstudio/ggvis/issues. If you're not sure if something is a
bug, you'd like to discuss new features or have any other questions about ggvis,
please join us on the mailing list: https://groups.google.com/group/ggvis.

## Getting started

To install: 

```R
devtools::install_github("hadley/testthat")
devtools::install_github("rstudio/ggvis")
```

Then check out the various demos in the `demo/` directory. Get started with
`demo/scatterplot.r` and then check out the the coolest demos, 
`demo/interactive.r` and `demo/tourr.r`.

## Vignettes

As well as the function level documentation, there are some vignettes (in 
`vignettes/`) that describe broader topics - start with `ggvis-basics`. 
Once you've installed the package you can list all vignettes with 
`vignette(package = "ggvis")`.

