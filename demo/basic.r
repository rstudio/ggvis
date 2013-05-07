

# Basic scatter plot
gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(stroke = "#000000", fill = "#000000")
)

# Line and point graph
gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_line(stroke = "#000000"),
  mark_point(stroke = "#000000", fill = "#000000")
)

# Two marks, at different levels of the tree
gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(stroke = "black", fill = "black"),
  node(
    mark_point(stroke = "black", fill = "red")
  )
)

# Two separate data sets, equal in the tree
mtc1 <- mtcars[1:10, ]
mtc2 <- mtcars[11:20, ]
gigvis(data = NULL, mapping = aes(x = "wt", y = "mpg"),
  node(
    data = "mtc1",
    mark_point(stroke = "black", fill = "black")
  ),
  node(
    data = "mtc2",
    mark_point(stroke = "black", fill = "red")
  )
)

# Scatter plot with linear model line
gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(),
  node(
    transform = transform_smooth(se = F),
    node(
      mapping = aes(x = "x", y = "y"),
      mark_line(stroke = "red")
    )
  )
)

# Scatter plot with linear model line for each level of cyl
gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(se = F),
    node(
      mapping = aes(x = "x", y = "y"),
      mark_line(stroke = "red")
    )
  )
)

# Histogram
gigvis("mtcars", aes(x = "wt"),
  transform = transform_bin(binwidth = 1),
  node(
    mapping = aes(x = "left", x2 = "right", y = 0, y2 = "count"),
    mark_rect()
  )
)
