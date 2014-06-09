library(ggvis)

# Basic scatter plot
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points()

# Variable transformations
mtcars %>% ggvis(x = ~wt, y = ~wt/mpg) %>% layer_points()
mtcars %>% ggvis(x = ~factor(cyl), y = ~mpg) %>% layer_points()

# With colour
# continuous:
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>% layer_points()
# and discrete:
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl)) %>%
  layer_points()

# Use unscaled constant: 10 refers to 10 pixels from top
mtcars %>% ggvis(x = ~wt) %>%
  layer_points(y = ~mpg) %>%
  layer_points(y := 10, fill := "red")

# Use scaled constant: 10 refers to data space
mtcars %>% ggvis(x = ~wt) %>%
  layer_points(y = ~mpg) %>%
  layer_points(y = 10, fill := "red")

# Line and point graph
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_lines() %>%
  layer_points(fill := "red")

# Two marks
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_points(fill := "red", size := 25)

# Two marks at different levels of the tree, with different mappings for a
# variable
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_points(fill := "red", y = ~qsec, size := 25)

# Two y scales
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_points(fill := "red", prop("y", ~qsec, scale = "yq"))

# Two separate data sets, equal in the tree
mtc1 <- mtcars[1:10, ]
mtc2 <- mtcars[11:20, ]
ggvis(NULL, x = ~wt, y = ~mpg) %>%
  layer_points(stroke := "black", fill := "black", data = mtc1) %>%
  layer_points(fill := "red", size := 40, data = mtc2)

# Scatter plot with one set of points with `cyl` mapped to stroke,
# and another set with `am` mapped to fill
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(stroke = ~factor(cyl), fill := NA) %>%
  layer_points(fill = ~factor(am), size := 25)

# Same as previous, but also with (useless) grouping in the layers
mtcars %>% group_by(cyl) %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(stroke = ~factor(cyl), fill := NA) %>%
  layer_points(fill = ~factor(am), size := 25)

# Use expression in a prop
pressure %>% ggvis(x = ~temperature, y = ~log(pressure)) %>% layer_points()

# Use variable from the calling environment
y_min <- min(log(pressure$pressure))
pressure %>% ggvis(x = ~temperature, y = ~log(pressure) - y_min) %>%
  layer_points()
