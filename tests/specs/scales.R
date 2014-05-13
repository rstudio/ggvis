# Modifying scales
library(ggvis)

set.seed(1014)
df <- data.frame(x = 1:5, y = 5:1, z = runif(5))

# Override default scale property
df %>%
  ggvis(x = ~x, y = ~x) %>%
  layer_points() %>%
  set_dscale("x", "numeric", trans = "log") %>%
  set_dscale("y", "numeric", trans = "log") %>%
  save_spec("scales/log.json")

# Override default scale name
df %>%
  ggvis(x = ~x, y = ~x, fill = prop(~z, scale = "blah")) %>%
  layer_points() %>%
  set_dscale("fill", "numeric", name = "blah") %>%
  save_spec("scales/custom.json")

# Dual scale
df %>%
  ggvis(x = ~x) %>%
  layer_points(y = prop(~y, scale = "y-y"), fill := "red") %>%
  layer_points(y = prop(~z, scale = "y-z")) %>%
  set_dscale("y", "numeric", name = "y-y") %>%
  set_dscale("y", "numeric", name = "y-z") %>%
  save_spec("scales/dual.json")

# Numeric domains
df %>%
  ggvis(x = ~x, y = ~x, fill = ~z) %>%
  layer_points() %>%
  set_dscale("x", "numeric", domain = c(0, 10)) %>%
  set_dscale("fill", "numeric", domain = c(0, 2)) %>%
  save_spec("scales/domain_numeric.json")
