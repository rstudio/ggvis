

# Basic scatter plot
gigvis("mtcars", c(x = "wt", y = "mpg"),
  mark_point()
)

# Scatter plot with linear model line
gigvis("mtcars", c(x = "wt", y = "mpg"),
  mark_point(),
  node(
    transform = transform_smooth(se = F),
    node(
      mapping = c(x = "x", y = "y"),
      mark_line(stroke = "red")
    )
  )
)

# Scatter plot with linear model line for each level of cyl
gigvis("mtcars", c(x = "wt", y = "mpg"),
  mark_point(),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(se = F),
    node(
      mapping = c(x = "x", y = "y"),
      mark_line(stroke = "red")
    )
  )
)

# Histogram
gigvis("mtcars", c(x = "wt"),
  transform = transform_bin(binwidth = 1),
  node(
    mapping = c(x = "left", x2 = "right", y = 0, y2 = "count"),
    mark_rect()
  )
)
