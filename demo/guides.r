library(ggvis)

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points()

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points() %>%
  add_axis("x", title = "Weight")

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points() %>%
  add_axis("x", title = "Weight", orient = "top")

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points() %>%
  add_legend("fill", title = "Cylinders")

# Combining two properties in one legend
mtcars %>%
  ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl), shape = ~factor(cyl)) %>%
  layer_points() %>%
  add_legend(c("fill", "shape"))
