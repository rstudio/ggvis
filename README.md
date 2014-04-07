# ggvis

[![Build Status](https://travis-ci.org/rstudio/ggvis.png?branch=master)](https://travis-ci.org/rstudio/ggvis)

The goal of ggvis is to make it easy to describe interactive web graphics in
R. It fuses the ideas of [ggplot2](http://github.com/hadley/ggplot2) and
[shiny](http://github.com/rstudio/shiny), rendering graphics on the web with
[vega](https://github.com/trifacta/vega) (so one can draw using [HTML5 canvas](http://diveintohtml5.info/canvas.html) or
[svg](http://en.wikipedia.org/wiki/Scalable_Vector_Graphics)). ggvis is less flexible than raw [d3](http://d3js.org/) or vega, but much more succinct and
is tailored to the needs of exploratory data analysis.

If you find a bug, please file a minimal reproducible example at
http://github.com/rstudio/ggvis/issues. If you're not sure if something is a
bug, you'd like to discuss new features or have any other questions about ggvis,
please join us on the mailing list: https://groups.google.com/group/ggvis.

## Getting started

ggvis is not yet available on CRAN, but you can install it directly from github with the following code.

```R
devtools::install_github(c("hadley/testthat", "rstudio/shiny", "rstudio/ggvis", "smbache/magrittr"))
```

Please keep in mind that the vignettes may not build properly on your system because they require not-yet-released rmarkdown package (as opposed to the existing markdown package available on CRAN). You can view built versions of the documents online, at http://ggvis.rstudio.com/

Also check out the
various demos in the `demo/` directory. Get started with `demo/scatterplot.r`
and then check out the the coolest demos, `demo/interactive.r` and
`demo/tourr.r`.
