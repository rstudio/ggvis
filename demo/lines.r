library(ggvis)
library(dplyr) # For arrange function

set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

df %>% ggvis(x = ~x, y = ~y) %>% layer_paths()

# Grouping, manually specified
df %>% group_by(z) %>%
  ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  layer_paths() %>%
  layer_points()

# Grouping can happen after ggvis() call
df %>%
  ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  group_by(z) %>%
  layer_paths() %>%
  layer_points()

# Data sorted by x
df %>% ggvis(x = ~x, y = ~y) %>%
  arrange(x) %>%
  layer_paths() %>%
  layer_points()

# layer_lines sorts and adds a mark_path
df %>% ggvis(x = ~x, y = ~y) %>%
  layer_lines() %>%
  layer_points()

# Data sorted by y
df %>% ggvis(x = ~x, y = ~y) %>%
  arrange(y) %>%
  layer_paths() %>%
  layer_points()

# Grouping with auto_group, and sorted
df %>% ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  auto_group() %>%
  arrange(x) %>%
  layer_paths() %>%
  layer_points()

# Using layer_lines, which sorts the data
df %>% ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  layer_lines() %>%
  layer_points()


# Dashed lines
dat <- data.frame(x = rep(c(0, 1), 6), g = gl(6, 2))
dat %>% group_by(g) %>%
  ggvis(x = ~x, y = ~g) %>%
  layer_paths(strokeDash = ~g) %>%
  add_axis("y", grid = FALSE) %>%
  add_axis("x", grid = FALSE, title = "", tick_size_major = 0, ticks = 0)
