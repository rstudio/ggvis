
# Scatter plot with loess model line
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(fill = NA, stroke = "black"),
  node(
    data = transform_smooth(se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)

# Scatter plot with lm model line
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(),
  node(
    data = transform_smooth(method = "lm"),
    mark_line(stroke = "red")
  )
)


# Scatter plot with linear model line for each level of factor(cyl)
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ factor(cyl), fill ~ factor(cyl)),
  mark_symbol(),
  scales = scales(
    scale(name = "stroke", type = "ordinal"),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    data = pipeline(
      by_group(variable(quote(factor(cyl)))),
      transform_smooth(method = "lm", se = F)
    ),
    mark_line(fill = NA)
  )
)

# Same as previous, but group by cyl instead of factor(cyl)
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ factor(cyl), fill ~ factor(cyl)),
  mark_symbol(),
  scales = scales(
    scale(name = "stroke", type = "ordinal"),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    data = pipeline(
      by_group(variable(quote(cyl))),
      transform_smooth(method = "lm", se = F)
    ),
    mark_line(fill = NA)
  )
)

# Scatter plot with loess lines for each level of factor(cyl), but the loess
# is based on a different y variable.
gigvis ("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(),
  node(
    data = pipeline(
      by_group(variable(quote(cyl))),
      transform_smooth()
    ),
    props = props(y ~ qsec),
    mark_line(stroke = "red")
  )
)


# Scatter plot with all black points and loess model line for each level of cyl
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(fill = "#000000"),
  scales = scales(scale(name = "stroke", type = "ordinal")),
  node(
    data = pipeline(
      by_group(variable(quote(factor(cyl)))),
      transform_smooth(se = F)
    ),
    props = props(stroke ~ factor(cyl)),
    mark_line()
  )
)


# Scatter plot with loess model lines on different y variables, with split by
# group
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(fill = "blue"),
  node(
    data = pipeline(
      by_group(variable(quote(cyl))),
      transform_smooth()
    ),
    mark_line(stroke = "blue")
  ),
  node(
    data = pipeline(
      by_group(variable(quote(cyl))),
      transform_smooth()
    ),
    props = props(y ~ qsec),
    mark_line(stroke = "red")
  )
)


# Scatter plot with linear and loess model line for each level of cyl
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ factor(cyl), fill ~ factor(cyl)),
  mark_symbol(),
  scales = scales(
    scale(name = "stroke", type = "ordinal"),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    data = pipeline(
      by_group(variable(quote(factor(cyl)))),
      transform_smooth(method = "lm", se = F)
    ),
    mark_line(fill = NA)
  ),
  node(
    data = pipeline(
      by_group(variable(quote(factor(cyl)))),
      transform_smooth(method = "loess", se = F)
    ),
    mark_line(fill = NA)
  )
)

# Scatter plot with two linear model lines for each level of cyl, with different
# mappings
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ factor(cyl), fill ~ factor(cyl)),
  mark_symbol(),
  scales = scales(
    scale(name = "stroke", type = "ordinal"),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    data = pipeline(
      by_group(variable(quote(factor(cyl)))),
      transform_smooth(method = "lm", se = F)
    ),
    mark_line(fill = NA)
  ),
  node(
    data = pipeline(
      by_group(variable(quote(factor(cyl)))),
      transform_smooth(method = "lm", se = F)
    ),
    props = props(y ~ qsec),
    mark_line(fill = NA, opacity = 0.25)
  )
)


# Bar graph with continuous x
gigvis(pressure,
  props = props(x ~ temperature, y ~ pressure),
  scales = scales(scale(name = "x", type = "linear")),
  mark_rect(y2 = constant(0, scale = TRUE), width = 15)
)


# Bar graph with ordinal x
gigvis("pressure",
  props = props(x ~ temperature, y ~ pressure),
  scales = scales(scale(name = "x", type = "linear")),
  mark_rect(y2 = constant(0, scale = TRUE), width = list(offset = -4))
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
