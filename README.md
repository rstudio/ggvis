# ggvis

[![Build Status](https://travis-ci.org/rstudio/ggvis.png?branch=master)](https://travis-ci.org/rstudio/ggvis)

The goal of ggvis is to make it easy to describe interactive web graphics in
R. It combines:

* a grammar of graphics from [ggplot2](http://github.com/hadley/ggplot2),
  
* reactive programming from [shiny](http://github.com/rstudio/shiny), and

* data transformation pipelines from [dplyr](http://github.com/hadley/dplyr).

ggvis graphics are rendered with [vega](https://github.com/trifacta/vega), so you can generate both raster graphics with [HTML5 canvas](http://diveintohtml5.info/canvas.html) and vector graphics with
[svg](http://en.wikipedia.org/wiki/Scalable_Vector_Graphics). ggvis is less flexible than raw [d3](http://d3js.org/) or vega, but is much more succinct and is tailored to the needs of exploratory data analysis.

If you find a bug, please file a minimal reproducible example at http://github.com/rstudio/ggvis/issues. If you're not sure if something is a bug, you'd like to discuss new features or have any other questions about ggvis, please join us on the mailing list: https://groups.google.com/group/ggvis.

## Installation 

ggvis is not yet available on CRAN. You can install it from github with the following code:

```R
# install.packages("devtools")
devtools::install_github(c("rstudio/rmarkdown", "rstudio/ggvis"),
    build_vignettes = FALSE)
```

## Getting started

You construct a visualisation by piping pieces together with `%>%`. The pipeline starts with a data set, flows into `ggvis()` to specify default visual properties, then layers on some visual elements:

```R
mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
```

The vignettes, available from http://ggvis.rstudio.com/, provide many more details. Start with the introduction, then work your way through the more advanced topics. Also check out the
various demos in the `demo/` directory. See the basics in `demo/scatterplot.r`
then check out the the coolest demos, `demo/interactive.r` and `demo/tourr.r`.
