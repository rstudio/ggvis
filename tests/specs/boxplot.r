library(ggvis)

mtcars %>%
  ggvis(~cyl, ~mpg) %>%
  layer_boxplots() %>%
  save_spec("boxplot/boxplot-continuous.json")

mtc <- mtcars
mtc$cyl <- factor(mtc$cyl)
mtc %>%
  ggvis(~cyl, ~mpg) %>%
  layer_boxplots() %>%
  save_spec("boxplot/boxplot-categorical.json")
