# ggvis

[![Build Status](https://travis-ci.org/rstudio/ggvis.png?branch=master)](https://travis-ci.org/rstudio/ggvis)

The goal of ggvis is to make it easy to describe interactive web graphics in
R. It fuses the ideas of [ggplot2](http://github.com/hadley/ggplot2) and
[shiny](http://github.com/rstudio/shiny), rendering graphics on the web with
[vega](https://github.com/trifacta/vega) (so can draw on the canvas or using
svg). It is less flexible than raw d3 or vega, but much more succinct and
is tailored to the needs of exploratory data analysis.

If you find a bug, please file a minimal reproducible example at
http://github.com/rstudio/ggvis/issues. If you're not sure if something is a
bug, you'd like to discuss new features or have any other questions about ggvis,
please join us on the mailing list: https://groups.google.com/group/ggvis.

## Getting started

ggvis is not yet available on CRAN, but you can install it directly from github with the following code.

```R
devtools::install_github(c("hadley/testthat", "rstudio/shiny", "rstudio/ggvis"))
```

Next, read the vignettes (`vignette(package = "ggvis")`), starting with
`vignette("ggvis-basics")`. They're still a work in progress, but should
help you understand the general layout of the package. Also check out the
various demos in the `demo/` directory. Get started with `demo/scatterplot.r`
and then check out the the coolest demos, `demo/interactive.r` and
`demo/tourr.r`.
