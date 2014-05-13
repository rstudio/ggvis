# Basic scatterplot
library(ggvis)

set.seed(1014)
df <- data.frame(x = 1:5, y = 5:1, z = runif(5))

df %>%
  ggvis(x = ~x, y = ~x) %>%
  layer_points() %>%
  save_spec("scatter/basic.json")

df %>%
  ggvis(x = ~factor(x), y = ~x / y) %>%
  layer_points() %>%
  save_spec("scatter/transform.json")

df %>%
  ggvis(x = ~x, y = ~y, fill = ~z) %>%
  layer_points() %>%
  save_spec("scatter/fill-continuous.json")

df %>%
  ggvis(x = ~x, y = ~y, fill = ~factor(z)) %>%
  layer_points() %>%
  save_spec("scatter/fill-discrete.json")
