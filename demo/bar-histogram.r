library(gigvis)

# Bar graph with continuous x
gigvis(pressure,
  props = props(x ~ temperature, y ~ pressure),
  mark_rect(y2 = constant(0, scale = TRUE), width = 15)
)

# Bar graph with ordinal x
gigvis("pressure",
  props = props(x ~ factor(temperature), y ~ pressure),
  mark_rect(y2 = constant(0, scale = TRUE), width = 15)
)

# Histogram, with specified width
gigvis(
  data = pipeline(mtcars, transform_bin(binwidth = 1)),
  props = props(x ~ wt),
  scales = scales(scale(name = "y", type = "linear", zero = TRUE)),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__,
      y2 = constant(0, scale = TRUE)),
    mark_rect()
  )
)

# Histogram, automatic binwidth
gigvis(
  data = pipeline(ggplot2::diamonds, transform_bin()),
  props = props(x ~ table),
  scales = scales(scale(name = "y", type = "linear", zero = TRUE)),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__,
      y2 = constant(0, scale = TRUE)),
    mark_rect()
  )
)

# Histogram, fill by cyl
gigvis(
  data = pipeline(
    "mtcars",
    by_group(variable(quote(factor(cyl)))),
    transform_bin(binwidth = 1)
  ),
  props = props(x ~ wt, fill ~ factor(cyl)),
  scales = scales(
    scale(name = "y", type = "linear", zero = TRUE),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__,
      y2 = constant(0, scale = TRUE)),
    mark_rect()
  )
)

# Histogram, fill by cut
diamonds <- ggplot2::diamonds
gigvis(
  data = pipeline(
    "diamonds",
    by_group(variable(quote(factor(cut)))),
    transform_bin()
  ),
  props = props(x ~ table, fill ~ factor(cut)),
  scales = scales(
    scale(name = "y", type = "linear", zero = TRUE),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__,
      y2 = constant(0, scale = TRUE)),
    mark_rect()
  )
)
