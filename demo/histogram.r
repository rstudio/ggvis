library(ggvis)

# Histogram, fully specified
ggvis(pipeline(mtcars, transform_bin(binwidth = 1)), props(x = ~wt)) +
  layer(
    props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0),
    mark_rect()
  )

# Or using shorthand layer
ggvis(mtcars, props(x = ~wt)) + layer_histogram(binwidth = 1)
ggvis(mtcars, props(x = ~wt)) + layer_histogram()

# Histogram, filled by cyl
by_cyl <- pipeline(mtcars, by_group(cyl))
ggvis(by_cyl, props(x = ~wt, fill = ~factor(cyl))) +
  layer_histogram(binwidth = 1)

ggvis(by_cyl, props(x = ~wt, stroke = ~factor(cyl))) +
  layer_freqpoly(binwidth = 1)


# Bigger dataset
data(diamonds, package = "ggplot2")
ggvis(diamonds, props(x = ~table)) +
  layer_histogram()


# Stacked histogram
ggvis(diamonds, by_group(cut), props(x = ~table, fill = ~cut),
  transform_bin(binwidth = 1)) +
  layer(
    props(x = ~xmin__, x2 = ~xmax__, y = ~count__, fillOpacity := 0.6),
    layer(
      transform_stack(),
      mark_rect(props(y = ~ymax__, y2 = ~ymin__))
    )
  )

# Histogram of dates
set.seed(2934)
dat <- data.frame(times = as.POSIXct("2013-07-01", tz = "GMT") + rnorm(200) * 60 * 60 * 24 * 7)
ggvis(dat, props(x = ~times)) + layer_histogram()
