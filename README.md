# ggvis

<!-- badges: start -->
[![R build status](https://github.com/wch/ggvis/workflows/R-CMD-check/badge.svg)](https://github.com/wch/ggvis/actions)
<!-- badges: end -->

## Status
![](https://img.shields.io/badge/lifecycle-dormant-blue.svg)
 
ggvis is currently dormant. We fundamentally believe in the ideas that underlie ggvis: reactive programming is the right foundation for interactive visualisation. However, we are not currently working on ggvis because we do not see it as the most pressing issue for the R community as you can only use interactive graphics once you've successfuly tackled the rest of the data analysis process.

We hope to come back to ggvis in the future; in the meantime you might want to try out [plotly](https://plotly.com/ggplot2/getting-started/) or creating inteactive graphics [with shiny](https://blog.rstudio.com/2015/06/16/shiny-0-12-interactive-plots-with-ggplot2/).

## Introduction

The goal of ggvis is to make it easy to describe interactive web graphics in
R. It combines:

* a grammar of graphics from [ggplot2](https://github.com/tidyverse/ggplot2),
  
* reactive programming from [shiny](https://github.com/rstudio/shiny), and

* data transformation pipelines from [dplyr](https://github.com/tidyverse/dplyr).

ggvis graphics are rendered with [vega](https://github.com/trifacta/vega), so you can generate both raster graphics with [HTML5 canvas](https://diveintohtml5.info/canvas.html) and vector graphics with
[svg](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics). ggvis is less flexible than raw [d3](https://d3js.org/) or vega, but is much more succinct and is tailored to the needs of exploratory data analysis.

If you find a bug, please file a minimal reproducible example at https://github.com/rstudio/ggvis/issues. If you're not sure if something is a bug, you'd like to discuss new features or have any other questions about ggvis, please join us on the mailing list: https://groups.google.com/group/ggvis.

## Installation 

Install the latest release version from CRAN with:

```R
install.packages("ggvis")
```

Install the latest development version with:

```R
# install.packages("devtools")
devtools::install_github("hadley/lazyeval", build_vignettes = FALSE)
devtools::install_github("hadley/dplyr", build_vignettes = FALSE)
devtools::install_github("rstudio/ggvis", build_vignettes = FALSE)
```

## Getting started

You construct a visualisation by piping pieces together with `%>%`. The pipeline starts with a data set, flows into `ggvis()` to specify default visual properties, then layers on some visual elements:

```R
mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
```

The vignettes, available from https://ggvis.rstudio.com/, provide many more details. Start with the introduction, then work your way through the more advanced topics. Also check out the
various demos in the `demo/` directory. See the basics in `demo/scatterplot.r`
then check out the the coolest demos, `demo/interactive.r` and `demo/tourr.r`.
