# Basic bar graphs
library(ggvis)
library(dplyr, warn.conflicts = FALSE)

pressure %>%
  ggvis(~factor(temperature), ~pressure) %>%
  layer_bars(width = 0.5) %>%
  save_spec("bar/categorical-x.json")

pressure %>%
  ggvis(x = ~temperature, y = ~pressure) %>%
  layer_bars(fill:="red") %>%
  save_spec("bar/continuous-x.json")
