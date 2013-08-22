library(ggvis)

# Bar graph with continuous x
ggvis(pressure,
  props(x ~ temperature, y ~ pressure),
  mark_rect(props(y2 ~ 0, width = 15))
)

# Bar graph with ordinal x
ggvis(pressure,
  props(x ~ temperature, y ~ pressure),
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE),
  mark_rect(props(y2 ~ 0, width = band()))
)

# Histogram, fully specified
ggvis(
  data = pipeline(mtcars, transform_bin(binwidth = 1)),
  props(x ~ wt),
  node(
    props(x ~ xmin__, x2 ~ xmax__, y ~ count__, y2 ~ 0),
    mark_rect()
  )
)

# Or using shorthand branch
ggvis(mtcars, props(x ~ wt),
  branch_histogram(binwidth = 1)
)
ggvis(mtcars, props(x ~ wt),
  branch_histogram()
)

# Histogram, filled by cyl
by_cyl <- pipeline(mtcars, by_group("cyl"))
ggvis(by_cyl, props(x ~ wt, fill ~ factor(cyl)),
  branch_histogram(binwidth = 1))

ggvis(by_cyl, props(x ~ wt, stroke ~ factor(cyl)),
  branch_freqpoly(binwidth = 1))


# Bigger dataset
data(diamonds, package = "ggplot2")
ggvis(diamonds, props(x ~ table),
  branch_histogram()
)

