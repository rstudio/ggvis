library(ggvis)

# Set size to 300x300 pixels
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  set_options(width = 300, height = 300)

# Set size to 300x300 pixels, and add 50 pixels padding on all sides
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  set_options(width = 300, height = 300, padding = padding(50, 50, 50, 50))
