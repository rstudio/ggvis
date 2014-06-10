library(ggvis)

# Discrete colours for fill and a manual scale for opacity
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl), fillOpacity = ~hp) %>%
  layer_points() %>%
  scale_numeric("opacity", range = c(0.2, 1))

# Control the domain of a scale - the y scale will go from 0 to whatever the
# maximum of the data is.
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points() %>%
  scale_numeric("y", domain = c(0, NA))

# Control the y range with a slider
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points() %>%
  scale_numeric("y", domain = input_slider(0, 50, c(10, 40), label = "Y range"))

# Control the lower y range with a slider
# FIXME: clamp=TRUE is necessary to work around a sizing bug
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points() %>%
  scale_numeric("y", clamp = TRUE,
    domain = input_slider(0, 50, label = "Lower Y",
                          map = function(x) c(x, NA))
  )

# Multiple x scales
mtcars %>% ggvis(y = ~mpg, size := 25) %>%
  layer_points(prop("x", quote(disp), scale = "xdisp")) %>%
  layer_points(prop("x", quote(wt), scale = "xwt"), fill := "red") %>%
  add_axis("x", "xdisp", orient = "top") %>%
  add_axis("x", "xwt", orient = "bottom",
    properties = axis_props(
      ticks = list(stroke = "red"),
      labels = list(fill = "red")
    )
  )


# Unscaled values in the data
mtc <- mtcars
mtc$color <- c("red", "teal", "#cccccc", "tan")
mtc %>% ggvis(x = ~wt, y = ~mpg, fill := ~color) %>% layer_points()

# Unscaled constant
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill := "red") %>% layer_points()
