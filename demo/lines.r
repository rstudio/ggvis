library(ggvis)

set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

df %>% ggvis(x = ~x, y = ~y) %>% mark_path()

# Grouping, manually specified
df %>% group_by(z) %>%
  ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  mark_path() %>%
  layer_point()

# Grouping can happen after ggvis() call
df %>%
  ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  group_by(z) %>%
  mark_path() %>%
  layer_point()

# Data sorted by x
df %>% ggvis(x = ~x, y = ~y) %>%
  transform_sort() %>%
  mark_path() %>%
  layer_point()

# layer_line sorts and adds a mark_path
df %>% ggvis(x = ~x, y = ~y) %>%
  layer_line() %>%
  layer_point()

# Data sorted by y
df %>% ggvis(x = ~x, y = ~y) %>%
  transform_sort(vars = "y") %>%
  mark_path() %>%
  layer_point()

# Grouping with auto_group, and sorted
df %>% ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  auto_group() %>%
  transform_sort() %>%
  mark_path() %>%
  layer_point()

# Using layer_line, which sorts the data
df %>% ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  layer_line() %>%
  layer_point()
