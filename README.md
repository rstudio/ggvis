# gigvis

The goal of gigvis is to makes it easy to describe interactive web graphics in 
R. It fuses the ideas of [ggplot2](http://github.com/hadley/ggplot2) and 
[shiny](http://github.com/rstudio/shiny), and it renders graphics with
[vega](https://github.com/trifacta/vega) (so can draw on the canvas or using
svg). It is less flexible than raw d3 or vega, but is also less verbose, and
tailored around the needs of exploratory data graphics.

## Getting started

To install: 

```R
library(devtools)
install_github(c("assertthat", "testthat"))
install_github("gigvis", "rstudio")
```

Currently, gigvis also needs the development version of httpuv, which can't
be install with `install_github`. Instead, run the following lines from the 
command line:

```R
git clone https://github.com/rstudio/httpuv
cd httpuv
git submodule update --init
R CMD INSTALL .
```

Then check out the various demos in the `demos/` directory.

