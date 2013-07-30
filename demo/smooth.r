library(gigvis)

# Scatter plot with loess model line
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill = NA, stroke = "black")),
  node(
    data = transform_smooth(se = F),
    node(
      mark_line(props(stroke = "red"))
    )
  )
)

# Or with shorthand branch_smooth
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill = NA, stroke = "black")),
  branch_smooth(props(stroke = "red"))
)

# Scatter plot with lm model line
gigvis("mtcars", props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(props(stroke = "red"), method = "lm")
)

# Scatter plot with linear model line for each level of cyl
by_cyl <- pipeline("mtcars", by_group("cyl"))
gigvis(by_cyl, props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill ~ factor(cyl))),
  branch_smooth(props(stroke ~ factor(cyl)))
)

# Scatter plot with loess lines for each level of cyl, but the loess
# is based on a different y variable.
gigvis(by_cyl, props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill ~ factor(cyl))),
  node(
    props(y ~ qsec, stroke ~ factor(cyl))
    branch_smooth()
  )
)

# Scatter plot with linear and loess model line for each level of cyl
gigvis(by_cyl, props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill ~ factor(cyl))),
  branch_smooth(method = "loess"),
  branch_smooth(method = "lm")
)

