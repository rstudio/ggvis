# Convert gigvis to vega and print resulting JSON to terminal
vegafy <- function(p) {
  cat(toJSON(vega_spec(p), pretty = TRUE), "\n")
}


# Basic scatter plot
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(stroke = "#000000", fill = "#000000")
)
vegafy(p)

# Line and point graph
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_line(stroke = "#000000"),
  mark_point(stroke = "#000000", fill = "#000000")
)
vegafy(p)

# Two marks, at different levels of the tree
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(stroke = "black", fill = "black"),
  node(
    mark_point(stroke = "black", fill = "red")
  )
)
vegafy(p)

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
    mark_point(stroke = "black", fill = "red")
  )
)
vegafy(p)


# Scatter plot with loess model line
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(),
  node(
    transform = transform_smooth(se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)
vegafy(p)

# Scatter plot with lm model line
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
  mark_point(stroke = "black", fill = "black"),
  node(
    transform = transform_smooth(method = "lm", se = F),
    node(
      mark_line(stroke = "red")
    )
  )
)
vegafy(p)

# Scatter plot with linear model line for each level of cyl
p <- gigvis("mtcars", aes(x = "wt", y = "mpg"),
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
vegafy(p)

# Histogram
p <- gigvis("mtcars", aes(x = "wt"),
  transform = transform_bin(binwidth = 1),
  node(
    mapping = aes(x = "left", x2 = "right", y = 0, y2 = "count"),
    mark_rect()
  )
)
vegafy(p)
