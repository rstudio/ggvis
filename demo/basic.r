library(gigvis)

# Basic scatter plot
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point()
)
view_static(p)

# Line and point graph
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_line(),
  mark_point(fill = "red")
)
view_static(p)

# Two marks, at different levels of the tree
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(),
  node(
    mark_point(fill = "red", size = 25)
  )
)
view_static(p)

# Two separate data sets, equal in the tree
mtc1 <- mtcars[1:10, ]
mtc2 <- mtcars[11:20, ]
p <- gigvis(data = NULL, mapping = aes(x = "wt", y = "mpg"),
  node(
    data = "mtc1",
    mark_point(stroke = "black", fill = "black")
  ),
  node(
    data = "mtc2",
    mark_point(fill = "red", size = 40)
  )
)
view_static(p)


# Scatter plot with loess model line
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(fill = NA, stroke = "black"),
  node(
    transform = transform_smooth(se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)
view_static(p)

# Scatter plot with lm model line
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(),
  node(
    transform = transform_smooth(method = "lm", se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)
view_static(p)



# Scatter plot, colored by cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg", fill = "cyl"),
  scales = list(fill = scale(name = "fill", type = "ordinal")),
  mark_point()
)
view_static(p)


# Scatter plot with linear model line for each level of cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg", stroke = "cyl", fill = "cyl"),
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
view_static(p)


# Scatter plot with all black points and loess model line for each level of cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(fill = "#000000"),
  scales = list(stroke = scale(name = "stroke", type = "ordinal")),
  node(
    split = by_group("cyl"),
    mapping = aes(stroke = "cyl"),
    transform = transform_smooth(se = F),
    mark_line()
  )
)
view_static(p)

# Scatter plot with linear and loess model line for each level of cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg", stroke = "cyl", fill = "cyl"),
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
view_static(p)

# Scatter plot with two linear model lines for each level of cyl, with different
# mappings
p <- gigvis("mtcars", aes(x = "wt", y = "mpg", stroke = "cyl", fill = "cyl"),
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
    mapping = aes(y = "qsec"),
    mark_line(fill = NA, opacity = 0.25)
  )
)
view_static(p)


# Bar graph with continuous x
p <- gigvis("pressure",
  mapping = aes(x = "temperature", y = "pressure"),
  scales = list(x = scale(name = "x", type = "linear")),
  mark_rect(y2 = 0, width = 15)
)
view_static(p)


# Bar graph with ordinal x
p <- gigvis("pressure",
  mapping = aes(x = "temperature", y = "pressure"),
  scales = list(x = scale(name = "x", type = "ordinal")),
  mark_rect(y2 = 0, width = list(offset = -4))
)
view_static(p)


# Histogram, with specified width
p <- gigvis("mtcars",
  mapping = aes(x = "wt"),
  transform = transform_bin(binwidth = 1),
  scales = list(y = scale(name = "y", type = "linear", zero = TRUE)),
  node(
    mapping = aes(x = "xmin__", x2 = "xmax__", y = "count__"),
    mark_rect(y2 = 0)
  )
)
view_static(p)

# Histogram, automatic binwidth
diamonds <- ggplot2::diamonds
p <- gigvis("diamonds",
  mapping = aes(x = "table"),
  transform = transform_bin(),
  scales = list(y = scale(name = "y", type = "linear", zero = TRUE)),
  node(
    mapping = aes(x = "xmin__", x2 = "xmax__", y = "count__"),
    mark_rect(y2 = 0)
  )
)
view_static(p)

# Histogram, fill by cyl
p <- gigvis("mtcars",
  mapping = aes(x = "wt", fill = "cyl"),
  transform = transform_bin(binwidth = 1),
  split = by_group("cyl"),
  scales = list(
    y = scale(name = "y", type = "linear", zero = TRUE),
    fill = scale(name = "fill", type = "ordinal")
  ),
  node(
    mapping = aes(x = "xmin__", x2 = "xmax__", y = "count__"),
    mark_rect(y2 = 0)
  )
)
view_static(p)

# Histogram, fill by cut
diamonds <- ggplot2::diamonds
p <- gigvis("diamonds",
  mapping = aes(x = "table", fill = "cut"),
  split = by_group("cut"),
  transform = transform_bin(),
  scales = list(
    y = scale(name = "y", type = "linear", zero = TRUE),
    fill = scale(name = "fill", type = "ordinal")
  ),
  node(
    mapping = aes(x = "xmin__", x2 = "xmax__", y = "count__"),
    mark_rect(y2 = 0)
  )
)
view_static(p)
