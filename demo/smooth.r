library(ggvis)

# Scatter plot with loess model line
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_point() %>%
  transform_smooth(se = F) %>%
  mark_path(props(x = ~x, y = ~y, stroke := "red"))

# Or with shorthand layer_smooth
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_point() %>%
  layer_smooth(props(stroke := "red"))

# With confidence region
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_point() %>%
  layer_smooth(props(stroke := "red"), se = TRUE)

# Scatter plot with lm model line
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_point() %>%
  layer_smooth(props(stroke := "red"), method = "lm")

# Scatterplot with lm and loess
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_point() %>%
  layer_smooth(props(stroke := "blue")) %>%
  layer_smooth(props(stroke := "red"), method = "lm")

# Scatter plot with linear model for each level of cyl
mtcars %>% ggvis(x = ~wt, y = ~mpg, stroke = ~factor(cyl)) %>%
  group_by(cyl) %>%
  layer_point() %>%
  layer_smooth(method = "lm")

# Scatter plot with linear model for each level of cyl, but only points coloured
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  group_by(cyl) %>%
  layer_point(props(fill = ~factor(cyl))) %>%
  layer_smooth(method = "lm")
