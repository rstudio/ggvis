library(ggvis)

# Scatter plot with loess model line
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill = NA, stroke = "black")),
  node(
    data = transform_smooth(se = F),
    node(
      mark_line(props(stroke = "red"))
    )
  )
)

# Or with shorthand branch_smooth
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(props(fill = NA, stroke = "black")),
  branch_smooth(props(stroke = "red"))
)

# Scatter plot with lm model line
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(props(stroke = "red"), method = "lm")
)

# Scatterplot with lm and loess
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(props(stroke = "blue")),
  branch_smooth(props(stroke = "red"), method = "lm")
)

# Scatter plot with linear model for each level of cyl
by_cyl <- pipeline(mtcars, by_group("cyl"))
ggvis(by_cyl, props(x ~ wt, y ~ mpg, stroke ~ factor(cyl)),
  mark_symbol(),
  branch_smooth(method = "lm")
)

# Scatter plot with linear model for each level of cyl, but only points coloured
by_cyl <- pipeline(mtcars, by_group("cyl"))
ggvis(by_cyl, props(x ~ wt, y ~ mpg),
  mark_symbol(props(stroke ~ factor(cyl))),
  branch_smooth(method = "lm")
)
