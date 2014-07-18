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
  ggvis(x = ~x, y = ~x, prop("fill", ~z, scale = "blah")) %>%
  layer_points() %>%
  add_legend("blah") %>%
  save_spec("scales/custom.json")

# Dual scale
df %>%
  ggvis(x = ~x) %>%
  layer_points(prop("y", ~y, scale = "y-y"), fill := "red") %>%
  layer_points(prop("y", ~z, scale = "y-z")) %>%
  save_spec("scales/dual.json")

# Numeric domains
df %>%
  ggvis(x = ~x, y = ~x, fill = ~z) %>%
  layer_points() %>%
  scale_numeric("x", domain = c(0, 10)) %>%
  scale_numeric("fill", domain = c(0, 2)) %>%
  save_spec("scales/domain_numeric.json")

# Datetimes
dat <- data.frame(
  time = as.POSIXct("2013-07-01", tz = "GMT") + rnorm(40) * 60 * 60 * 24 * 7,
  value = rnorm(40)
)

dat %>% ggvis(~time, ~value) %>% layer_points() %>%
  save_spec("scales/datetime.json")

dat %>% ggvis(~time) %>% layer_histograms() %>%
  save_spec("scales/datetime_hist.json")

# Two properties in one legend, one on a custom scale
df %>%
  ggvis(x = ~x, y = ~x, fill = ~factor(y), prop("shape", ~factor(y), "shape2")) %>%
  layer_points() %>%
  add_legend(c("fill", "shape2")) %>%
  save_spec("scales/combined_legend.json")

# Nominal x with bars
df %>%
  ggvis(x = ~factor(x), y = ~y) %>%
  layer_bars() %>%
  save_spec("scales/bars.json")

# Hide axes and legends
df %>%
  ggvis(x = ~x, y = ~y, fill = ~z, shape = ~factor(x)) %>%
  layer_points() %>%
  hide_legend("fill") %>%
  hide_legend("shape") %>%
  hide_axis("x") %>%
  hide_axis("y") %>%
  save_spec("scales/hide_guides.json")
