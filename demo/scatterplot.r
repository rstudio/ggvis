library(ggvis)

# Basic scatter plot
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>% layer_point()

# Variable transformations
mtcars %>% ggvis(props(x = ~wt, y = ~wt/mpg)) %>% layer_point()
mtcars %>% ggvis(props(x = ~factor(cyl), y = ~mpg)) %>% layer_point()

# With colour
# continuous:
mtcars %>% ggvis(props(x = ~wt, y = ~mpg, fill = ~cyl)) %>% layer_point()
# and discrete:
mtcars %>% ggvis(props(x = ~wt, y = ~mpg, fill = ~factor(cyl))) %>%
  layer_point()

# Use unscaled constant: 10 refers to 10 pixels from top
mtcars %>% ggvis(props(x = ~wt)) %>%
  layer_point(props(y = ~mpg)) %>%
  layer_point(props(y := 10, fill := "red"))

# Use scaled constant: 10 refers to data space
mtcars %>% ggvis(props(x = ~wt)) %>%
  layer_point(props(y = ~mpg)) %>%
  layer_point(props(y = 10, fill := "red"))

# Line and point graph
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>%
  layer_line() %>%
  layer_point(props(fill := "red"))

# Two marks
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>%
  layer_point() %>%
  layer_point(props(fill := "red", size := 25))

# Multiple nested layers
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>%
  branch(layer_point()) %>%
  branch(branch(layer_point(props(fill := "red", size := 25))))

# Two marks at different levels of the tree, with different mappings for a
# variable
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>%
  layer_point() %>%
  layer_point(props(fill := "red", y = ~qsec, size := 25))

# Two y scales
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>%
  layer_point() %>%
  layer_point(props(fill := "red", y = prop(~ qsec, scale = "yq"))) %>%
  dscale("y", "numeric", name = "yq")

# Two separate data sets, equal in the tree
mtc1 <- mtcars[1:10, ]
mtc2 <- mtcars[11:20, ]
ggvis(NULL, props(x = ~wt, y = ~mpg)) %>%
  layer_point(props(stroke := "black", fill := "black"), data = mtc1) %>%
  layer_point(props(fill := "red", size := 40), mtc2)

# Scatter plot with one set of points with `cyl` mapped to stroke,
# and another set with `am` mapped to fill
mtcars %>% ggvis(props(x = ~wt, y = ~mpg)) %>%
  layer_point(props(stroke = ~factor(cyl), fill := NA)) %>%
  layer_point(props(fill = ~factor(am), size := 25))

# Same as previous, but also with (useless) grouping in the layers
mtcars %>% group_by(cyl) %>%
  ggvis(props(x = ~wt, y = ~mpg)) %>%
  layer_point(props(stroke = ~factor(cyl), fill := NA)) %>%
  layer_point(props(fill = ~factor(am), size := 25))

# Use expression in a prop
pressure %>% ggvis(props(x = ~temperature, y = ~log(pressure))) %>% layer_point()

# Use variable from the calling environment
y_min <- min(log(pressure$pressure))
pressure %>% ggvis(props(x = ~temperature, y = ~log(pressure) - y_min)) %>%
  layer_point()
