
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

