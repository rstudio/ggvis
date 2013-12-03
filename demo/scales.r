library(ggvis)

# Discrete colours for fill and a manual scale for opacity
ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~factor(cyl), fillOpacity = ~hp)) +
  mark_symbol() +
  dscale("opacity", "numeric", range = c(0.2, 1))

# Unscaled values in the data
mtc <- mtcars
mtc$color <- c("red", "teal", "#cccccc", "tan")
ggvis(mtc, props(x = ~wt, y = ~mpg, fill := ~color)) + mark_symbol()

# Unscaled constant
ggvis(mtcars, props(x = ~wt, y = ~mpg, fill := "red")) + mark_symbol()
