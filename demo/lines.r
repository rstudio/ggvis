library(ggvis)

set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

df %>% ggvis(props(x = ~x, y = ~y)) %>% mark_path()

# Grouping, manually specified
df %>% group_by(z) %>%
  ggvis(props(x = ~x, y = ~y, stroke = ~z, fill := NA)) %>%
  mark_path() %>%
  layer_point()

df %>%
  ggvis(props(x = ~x, y = ~y, stroke = ~z, fill := NA)) %>%
  group_by(z) %>%
  mark_path() %>%
  layer_point()

# Data sorted by x
df %>% ggvis(props(x = ~x, y = ~y)) %>%
  transform_sort() %>%
  mark_path() %>%
  layer_point()

# Data sorted by y
df %>% ggvis(props(x = ~x, y = ~y)) %>%
  transform_sort(vars = "y") %>%
  mark_path() %>%
  layer_point()

# Grouping with auto_group, and sorted
df %>% ggvis(props(x = ~x, y = ~y, stroke = ~z, fill := NA)) %>%
  auto_group() %>%
  transform_sort() %>%
  mark_path() %>%
  layer_point()

# Using layer_line
df %>% ggvis(df, props(x = ~x, y = ~y, stroke = ~z, fill := NA)) %>%
  layer_line() %>%
  layer_point()
