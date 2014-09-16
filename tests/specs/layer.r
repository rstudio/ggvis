library(ggvis)

mtcars %>%
  ggvis(x = ~wt) %>%
  layer_histograms(width = 1) %>%
  save_spec("layer/histogram.json")

mtcars %>%
  ggvis(x = ~wt, stroke = ~cyl) %>%
  group_by(cyl) %>%
  layer_freqpolys(width = 1) %>%
  save_spec("layer/freqpoly-grouped.json")

mtcars %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  layer_smooths() %>%
  save_spec("layer/smooth.json")

mtcars %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  group_by(cyl) %>%
  layer_smooths() %>%
  save_spec("layer/smooth-grouped.json")
