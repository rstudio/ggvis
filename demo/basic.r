library(gigvis)

# Basic scatter plot
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_point()
)

# Line and point graph
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_line(),
  mark_point(fill = "red")
)

# Two marks, at different levels of the tree
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_point(),
  node(
    mark_point(fill = "red", size = 25)
  )
)

# Two separate data sets, equal in the tree
mtc1 <- mtcars[1:10, ]
mtc2 <- mtcars[11:20, ]
gigvis(data = NULL, props = props(x ~ wt, y ~ mpg),
  node(
    data = "mtc1",
    mark_point(stroke = "black", fill = "black")
  ),
  node(
    data = "mtc2",
    mark_point(fill = "red", size = 40)
  )
)


# Scatter plot with loess model line
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_point(fill = NA, stroke = "black"),
  node(
    transform = transform_smooth(se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)

# Scatter plot with lm model line
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_point(),
  node(
    transform = transform_smooth(method = "lm", se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)


# Scatter plot, colored by cyl
gigvis("mtcars", props(x ~ wt, y ~ mpg, fill ~ cyl),
  scales = list(fill = scale(name = "fill", type = "ordinal")),
  mark_point()
)


# Scatter plot with linear model line for each level of cyl
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ cyl, fill ~ cyl),
  mark_point(),
  scales = list(
    stroke = scale(name = "stroke", type = "ordinal"),
    fill  = scale(name = "fill", type = "ordinal")
  ),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(method = "lm", se = F),
    mark_line(fill = NA)
  )
)


# Scatter plot with all black points and loess model line for each level of cyl
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_point(fill = "#000000"),
  scales = list(stroke = scale(name = "stroke", type = "ordinal")),
  node(
    split = by_group("cyl"),
    props = props(stroke ~ cyl),
    transform = transform_smooth(se = F),
    mark_line()
  )
)

# Scatter plot with linear and loess model line for each level of cyl
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ cyl, fill ~ cyl),
  mark_point(),
  scales = list(
    stroke = scale(name = "stroke", type = "ordinal"),
    fill  = scale(name = "fill", type = "ordinal")
  ),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(method = "lm", se = F),
    mark_line(fill = NA)
  ),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(method = "loess", se = F),
    mark_line(fill = NA)
  )
)

# Scatter plot with two linear model lines for each level of cyl, with different
# mappings
gigvis("mtcars", props(x ~ wt, y ~ mpg, stroke ~ cyl, fill ~ cyl),
  mark_point(),
  scales = list(
    stroke = scale(name = "stroke", type = "ordinal"),
    fill  = scale(name = "fill", type = "ordinal")
  ),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(method = "lm", se = F),
    mark_line(fill = NA)
  ),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(method = "lm", se = F),
    props = props(y ~ qsec),
    mark_line(fill = NA, opacity = 0.25)
  )
)


# Bar graph with continuous x
gigvis("pressure",
  props = props(x ~ temperature, y ~ pressure),
  scales = list(x = scale(name = "x", type = "linear")),
  mark_rect(y2 = 0, width = 15)
)


# Bar graph with ordinal x
gigvis("pressure",
  props = props(x ~ temperature, y ~ pressure),
  scales = list(x = scale(name = "x", type = "ordinal")),
  mark_rect(y2 = 0, width = list(offset = -4))
)


# Histogram, with specified width
gigvis("mtcars",
  props = props(x ~ wt),
  transform = transform_bin(binwidth = 1),
  scales = list(y = scale(name = "y", type = "linear", zero = TRUE)),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__),
    mark_rect(y2 = 0)
  )
)

# Histogram, automatic binwidth
diamonds <- ggplot2::diamonds
gigvis("diamonds",
  props = props(x ~ table),
  transform = transform_bin(),
  scales = list(y = scale(name = "y", type = "linear", zero = TRUE)),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__),
    mark_rect(y2 = 0)
  )
)

# Histogram, fill by cyl
gigvis("mtcars",
  props = props(x ~ wt, fill ~ cyl),
  transform = transform_bin(binwidth = 1),
  split = by_group("cyl"),
  scales = list(
    y = scale(name = "y", type = "linear", zero = TRUE),
    fill = scale(name = "fill", type = "ordinal")
  ),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__),
    mark_rect(y2 = 0)
  )
)

# Histogram, fill by cut
diamonds <- ggplot2::diamonds
gigvis("diamonds",
  props = props(x ~ table, fill ~ cut),
  split = by_group("cut"),
  transform = transform_bin(),
  scales = list(
    y = scale(name = "y", type = "linear", zero = TRUE),
    fill = scale(name = "fill", type = "ordinal")
  ),
  node(
    props = props(x ~ xmin__, x2 ~ xmax__, y ~ count__),
    mark_rect(y2 = 0)
  )
)
