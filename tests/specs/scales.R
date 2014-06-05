# Modifying scales
library(ggvis)

set.seed(1014)
df <- data.frame(x = 1:5, y = 5:1, z = runif(5))

# Override default scale property
df %>%
  ggvis(x = ~x, y = ~x) %>%
  layer_points() %>%
  scale_numeric("x", trans = "log") %>%
  scale_numeric("y", trans = "log") %>%
  save_spec("scales/log.json")

# Override default scale name
df %>%
  ggvis(x = ~x, y = ~x, fill = prop(~z, scale = "blah")) %>%
  layer_points() %>%
  add_guide_legend(fill = "blah") %>%
  save_spec("scales/custom.json")

# Dual scale
df %>%
  ggvis(x = ~x) %>%
  layer_points(y = prop(~y, scale = "y-y"), fill := "red") %>%
  layer_points(y = prop(~z, scale = "y-z")) %>%
  scale_numeric("y", name = "y-y") %>%
  scale_numeric("y", name = "y-z") %>%
  save_spec("scales/dual.json")

# Numeric domains
df %>%
  ggvis(x = ~x, y = ~x, fill = ~z) %>%
  layer_points() %>%
  scale_numeric("x", domain = c(0, 10)) %>%
  scale_numeric("fill", domain = c(0, 2)) %>%
  save_spec("scales/domain_numeric.json")
