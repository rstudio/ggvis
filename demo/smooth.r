library(ggvis)

# Scatter plot with loess model line
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  layer(
    transform_smooth(se = F),
    layer(
      mark_line(props(x = ~x, y = ~y, stroke := "red"))
    )
  )

# Or with shorthand layer_smooth
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  layer_smooth(props(stroke := "red"))

# Scatter plot with lm model line
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  layer_smooth(props(stroke := "red"), method = "lm")

# Scatterplot with lm and loess
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  layer_smooth(props(stroke := "blue"), se = FALSE) +
  layer_smooth(props(stroke := "red"), method = "lm", se = FALSE)

# Scatter plot with linear model for each level of cyl
ggvis(mtcars, by_group(cyl), props(x = ~wt, y = ~mpg, stroke = ~factor(cyl))) +
  mark_symbol() +
  layer_smooth(method = "lm")

# Scatter plot with linear model for each level of cyl, but only points coloured
ggvis(mtcars, by_group(cyl), props(x = ~wt, y = ~mpg)) +
  mark_symbol(props(fill = ~factor(cyl))) +
  layer_smooth(method = "lm")
