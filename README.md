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

## Compared to ggplot2

If you're familiar with ggplot2, learning gigvis shouldn't be too hard - it 
borrows from many familiar concepts.  Note that gigvis is still very young,
and many of the interfaces are likely to change as we learn more about what
works well.

* Basic naming conversions: layer -> node, geom -> mark, stat -> transform, 
  aes -> props.

* Facetting is not currently supported, and when it is supported, it's more
  like to resemble [embedded plots](http://vita.had.co.nz/papers/embedded-plots.html)
  than facetting in ggplot2.

* In ggplot2, the definition of a geom was somewhat blurred, because of things
  like `geom_histogram()` which combined `geom_bar()` with `stat_bin()`. The
  distinction is more clear in gigvis: pure geoms correspond to marks, and 
  combined geoms and stats correspond to branches.
  
* There is no equivalent to `qplot` in gigvis: you must spell out everything
  explicitly using `gigvis`.

* Vega provides a smaller set of scales than ggplot2 (just ordinal, 
  quantitative, and time), but they are much more flexible than ggplot2 scales,
  and offer equivalent functionality.

* ggplot2 has a two-level hierarchy - you have data and aes specifications in
  the plot and in each layer. gigvis provides an unlimited hierarchy - you can
  have as many levels as you need (and the data will only be computed once)
  
* gigvis makes fewer assumptions about the type of data - data does not have
  to be a data frame until it has been processed by a transform.
  
## Compared to vega/d3

While gigvis is built on top of vega, which in turn borrows many ideas from d3,
it is designed more for data exploration than data presentation. This means that
gigvis makes many more assumptions about what you're trying to do: this allows
it to be much more concise, at some cost of generality.

The main difference to vega is that gigvis provides a tree like structure 
allowing properties and data to be specified once and then inherited by 
children. 
