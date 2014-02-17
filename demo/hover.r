library(ggvis)

# Scatter plot with hovering
ggvis(mtcars, props(x = ~wt, y = ~mpg, size.hover := 200)) +
  layer_point()

# Larger point and outline when hovering
ggvis(mtcars,
  props(x = ~wt, y = ~mpg, size.hover := 200,
        stroke := NA, stroke.hover := "red", strokeWidth := 3)) +
  layer_point()

# Line changes color and points change size when hovered over, with 250 ms
# transition time
ggvis(pressure, props(x = ~temperature, y = ~pressure)) +
  mark_path(props(stroke.hover := "red", strokeWidth.hover := 4, strokeWidth := 2)) +
  layer_point(props(size := 50, size.hover := 200)) +
  opts(hover_duration = 250)

# Hover with transform_smooth
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  layer_point() +
  layer_smooth(props(fill.hover := "red"))

# Opacity with transform_density
ggvis(PlantGrowth, by_group(group),
  props(x = ~weight, stroke = ~group, fill = ~group, fillOpacity := 0.2, fillOpacity.hover := .5)) +
  layer_density()
