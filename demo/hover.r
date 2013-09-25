library(ggvis)

# Scatter plot with hovering
ggvis(mtcars, props(x = ~wt, y = ~mpg, size := 100, size.hover := 200),
  mark_symbol()
)

# Larger point and outline when hovering
ggvis(mtcars, 
  props(x = ~wt, y = ~mpg, size := 100, size.hover := 200,
        stroke := NA, stroke.hover := "red", strokeWidth := 3),
  mark_symbol()
)

# Line changes color and points change size when hovered over
ggvis(pressure, props(x = ~temperature, y = ~pressure),
  mark_line(props(stroke.hover := "red", strokeWidth.hover := 4, strokeWidth := 2)),
  mark_symbol(props(size := 50, size.hover := 200))
)
