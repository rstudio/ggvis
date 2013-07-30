library(gigvis)

# Bar graph with continuous x
gigvis(pressure,
  props = props(x ~ temperature, y ~ pressure),
  mark_rect(props(y2 = constant(0, scale = TRUE), width = 15))
)

# Bar graph with ordinal x
gigvis("pressure",
  props = props(x ~ temperature, y ~ pressure),
  mark_rect(props(y2 = constant(0, scale = TRUE), width = band())),
  scales = scales(scale_ordinal("x", range = "width", padding = 0, points = FALSE))
)

# Histogram, fully specified
gigvis(
  data = pipeline(mtcars, transform_bin(binwidth = 1)),
  props = props(x ~ wt),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__,
      y2 = constant(0, scale = TRUE)),
    mark_rect()
  )
)

# Or using shorthand branch
gigvis("mtcars", props(x ~ wt),
  branch_histogram(binwidth = 1)
)
gigvis("mtcars", props(x ~ wt),
  branch_histogram()
)

# Histogram, filled by cyl
by_cyl <- pipeline("mtcars", by_group("cyl"))
gigvis(by_cyl, props(x ~ wt, fill ~ factor(cyl)),
  branch_histogram(binwidth = 1))


# Bigger dataset
data(diamonds, package = "ggplot2")
gigvis("diamonds", props(x ~ table),
  branch_histogram()
)

