library(ggvis)

# Scatter plot with hovering
mtcars %>% ggvis(x = ~wt, y = ~mpg, size.hover := 200) %>%
  layer_points()

# Larger point and outline when hovering
mtcars %>%
  ggvis(x = ~wt, y = ~mpg, size.hover := 200,
        stroke := NA, stroke.hover := "red", strokeWidth := 3) %>%
  layer_points()

# Line changes color and points change size when hovered over, with 250 ms
# transition time
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  layer_paths(stroke.hover := "red", strokeWidth.hover := 4, strokeWidth := 2) %>%
  layer_points(size := 50, size.hover := 200) %>%
  set_options(hover_duration = 250)

# Hover with layer_smooths
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_smooths(fill.hover := "red", se = TRUE)

# Opacity with layer_densities
PlantGrowth %>% group_by(group) %>%
  ggvis(x = ~weight, stroke = ~group, fill = ~group,
        fillOpacity := 0.2, fillOpacity.hover := .5) %>%
  layer_densities()
