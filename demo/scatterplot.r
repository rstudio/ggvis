library(gigvis)

# Basic scatter plot
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol()
)

# Variable transformations
gigvis("mtcars", props(x ~ wt, y ~ wt/mpg),
  mark_symbol()
)
gigvis("mtcars", props(x ~ factor(cyl), y ~ mpg),
  mark_symbol()
)

# With colour
# continuous:
gigvis("mtcars", props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol()
)
# and discrete:
gigvis("mtcars", props(x ~ wt, y ~ mpg, fill ~ factor(cyl)),
  mark_symbol()
)

# Use unscaled constant: 10 refers to 10 pixels from top
gigvis("mtcars", props(x ~ wt),
  mark_symbol(y ~ mpg),
  mark_symbol(y = constant(10), fill = "red")
)

# Use scaled constant: 10 refers to data space
gigvis("mtcars", props(x ~ wt),
  mark_symbol(y ~ mpg),
  mark_symbol(y = constant(10, scale = TRUE), fill = "red")
)

# Line and point graph
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_line(),
  mark_symbol(fill = "red")
)

# Two marks
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(),
  mark_symbol(fill = "red", size = 25)
)

# Multiple nested nodes
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  node(mark_symbol()),
  node(node(mark_symbol(fill = "red", size = 25)))
)

# Two marks at different levels of the tree, with different mappings for a
# variable
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(),
  mark_symbol(fill = "red", y ~ qsec, size = 25)
)

# Two y scales
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(),
  mark_symbol(fill = "red", y = variable(quote(qsec), scale = "yq")),
  scales = scales(scale_quantitative("yq", range = "height"))
)

# Two separate data sets, equal in the tree
mtc1 <- mtcars[1:10, ]
mtc2 <- mtcars[11:20, ]
gigvis(data = NULL, props = props(x ~ wt, y ~ mpg),
  mark_symbol(data = "mtc1", stroke = "black", fill = "black"),
  mark_symbol(data = "mtc2", fill = "red", size = 40)
)

# Scatter plot with one set of points with `cyl` mapped to stroke, 
# and another set with `am` mapped to fill
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(stroke ~ factor(cyl), fill = NA),
  mark_symbol(fill ~ factor(am), size = 25)
)

# Same as previous, but also with (useless) grouping in the nodes
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  scales = scales(
    scale(name = "stroke", type = "ordinal"),
    scale(name = "fill", type = "ordinal")
  ),
  node(
    data = by_group(variable(quote(factor(cyl)))),
    props = props(stroke ~ factor(cyl)),
    mark_symbol(fill = NA)
  ),
  node(
    data = by_group(variable(quote(factor(am)))),
    props = props(fill ~ factor(am)),
    mark_symbol(size = 25)
  )
)
