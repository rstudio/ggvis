library(gigvis)

# Basic scatter plot
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(stroke = "#000000")
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
  mark_point(fill = NA),
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
p <- gigvis("mtcars", aes(x = "wt", y = "mpg", color = "cyl"),
  scales = list(color = scale(name = "color", type = "ordinal")),
  mark_point()
)
view_static(p)


# Scatter plot with linear model line for each level of cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg", color = "cyl", fill = "cyl"),
  mark_point(),
  scales = list(
    color = scale(name = "color", type = "ordinal"),
    fill  = scale(name = "fill", type = "ordinal")
  ),
  node(
    split = by_group("cyl"),
    transform = transform_smooth(method = "loess", se = F),
    mark_line(fill = NA)
  )
)
view_static(p)


# Scatter plot with all black points and loess model line for each level of cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(fill = "#000000"),
  scales = list(color = scale(name = "color", type = "ordinal")),
  node(
    split = by_group("cyl"),
    mapping = aes(color = "cyl"),
    transform = transform_smooth(se = F),
    mark_line()
  )
)
view_static(p)



# Bar graph
p <- gigvis("pressure",
  mapping = aes(x = "temperature", y = "pressure"),
  mark_rect()
)
view_static(p)


# Histogram
p <- gigvis("mtcars", aes(x = "wt"),
  transform = transform_bin(binwidth = 1),
  node(
    mapping = aes(x = "left", x2 = "right", y = 0, y2 = "count"),
    mark_rect()
  )
)
view_static(p)
