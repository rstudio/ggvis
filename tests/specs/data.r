# Data handling tests
library(ggvis)
library(dplyr, warn.conflicts = FALSE)

# Using "." in names of grouped and non-grouped vars
mtcars %>%
  dplyr::mutate(cyl.new = factor(cyl), mpg.new = mpg) %>%
  group_by(cyl.new) %>%
  ggvis(~wt, ~mpg.new, fill = ~cyl.new) %>%
  layer_points() %>%
  save_spec("data/dots.json")
