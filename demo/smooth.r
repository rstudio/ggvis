library(ggvis)

# Scatter plot with loess model line
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  compute_smooth(mpg ~ wt, se = F) %>%
  layer_paths(x = ~pred_, y = ~resp_, stroke := "red")

# Or with shorthand layer_smooth
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_smooths(stroke := "red")

# With confidence region
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_smooths(stroke := "red", se = TRUE)

# Scatter plot with lm model line
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_model_predictions(stroke := "red", model = "lm")

# Scatterplot with lm and loess
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_smooths(stroke := "blue") %>%
  layer_model_predictions(stroke := "red", model = "lm")

# Scatter plot with smooth for each level of cyl
mtcars %>% ggvis(x = ~wt, y = ~mpg, stroke = ~factor(cyl)) %>%
  group_by(cyl) %>%
  layer_points() %>%
  layer_smooths()

# Scatter plot with smooth for each level of cyl, but only points coloured
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  group_by(cyl) %>%
  layer_points(fill = ~factor(cyl)) %>%
  layer_smooths()
