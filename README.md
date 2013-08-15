# gigvis

The goal of gigvis is to makes it easy to describe interactive web graphics in 
R. It fuses the ideas of [ggplot2](http://github.com/hadley/ggplot2) and 
[shiny](http://github.com/rstudio/shiny), rendering graphics on the web with
[vega](https://github.com/trifacta/vega) (so can draw on the canvas or using
svg). It is less flexible than raw d3 or vega, but is also less verbose, and
tailored around the needs of exploratory data graphics.

## Getting started

To install: 

```R
library(devtools)
install_github(c("assertthat", "testthat"))
install_github(c("httpuv", "shiny", "gigvis"), "rstudio")
```

Then check out the various demos in the `demo/` directory. Get started with
`demo/scatterplot.r` and then check out the the coolest demos, 
`demo/interactive.r` and `demo/tourr.r`.

## Vignettes

As well as the function level documentation, there are some vignettes (in 
`vignettes/`) that describe broader topics.  Once you've installed the package
you can list all vignettes with `vignette(package = "gigvis")`.

