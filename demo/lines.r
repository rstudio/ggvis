library(ggvis)

set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

ggvis(df, props(x = ~x, y = ~y), mark_line())
ggvis(pipeline(df, by_group(z)), props(x = ~x, y = ~y, stroke = ~z), mark_line(), mark_symbol())

# Data sorted by x
ggvis(df, transform_sort(), props(x = ~x, y = ~y), mark_line(), mark_symbol())
# Data sorted by y
ggvis(df, transform_sort(var = "y"), props(x = ~x, y = ~y), mark_line(), mark_symbol())
