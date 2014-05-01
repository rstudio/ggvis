library(ggvis)

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points()

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points() %>%
  add_guide_axis("x", title = "Weight")

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points() %>%
  add_guide_axis("x", title = "Weight", orient = "top")

mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points() %>%
  add_guide_legend(fill = "fill", title = "Cylinders")
