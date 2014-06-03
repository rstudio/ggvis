# Basic line graphs
library(ggvis)
library(dplyr, warn.conflicts = FALSE)

set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4),
                 w = factor(rep(letters[1:4], 3)))

df %>%
  ggvis(x = ~x, y = ~y) %>%
  layer_paths() %>%
  save_spec("line/basic.json")

df %>%
  ggvis(x = ~x, y = ~y) %>%
  arrange(x) %>%
  layer_paths() %>%
  save_spec("line/sort.json")

df %>%
  ggvis(x = ~x, y = ~y, stroke = ~z) %>%
  layer_lines() %>%
  save_spec("line/layer-line.json")

# Auto-grouping for lines with nominal x
df %>%
  ggvis(x = ~w, y = ~y, stroke = ~z) %>%
  layer_lines() %>%
  save_spec("line/layer-line-nominal-x.json")
