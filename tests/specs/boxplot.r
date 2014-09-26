library(ggvis)

mtcars %>%
  ggvis(~cyl, ~mpg) %>%
  layer_boxplots() %>%
  save_spec("boxplot/boxplot-continuous.json")

mtcars %>%
  ggvis(~factor(cyl), ~mpg) %>%
  layer_boxplots() %>%
  save_spec("boxplot/boxplot-categorical.json")


dat <- data.frame(x = c('a','a','a','b','b','b'), y = c(1:3, 2:4))
dat %>%
  ggvis(x = ~x, y = ~y) %>%
  layer_boxplots() %>%
  save_spec("boxplot/boxplot-no-outliers.json")
