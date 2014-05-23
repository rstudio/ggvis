library(ggvis)

# Discrete colours for fill and a manual scale for opacity
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl), fillOpacity = ~hp) %>%
  layer_points() %>%
  set_dscale("opacity", "numeric", range = c(0.2, 1))

# Control the domain of a scale - the y scale will go from 0 to whatever the
# maximum of the data is.
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points() %>%
  set_dscale("y", "numeric", domain = c(0, NA))

# Control the y range with a slider
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points() %>%
  set_dscale("y", "numeric",
             domain = input_slider(0, 50, c(10, 40), label = "Y range"))

# Unscaled values in the data
mtc <- mtcars
mtc$color <- c("red", "teal", "#cccccc", "tan")
mtc %>% ggvis(x = ~wt, y = ~mpg, fill := ~color) %>% layer_points()

# Unscaled constant
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill := "red") %>% layer_points()
