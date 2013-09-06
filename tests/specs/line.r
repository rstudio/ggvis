# Basic line graphs
library(ggvis)

set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

ggvis(df, props(x = ~x, y = ~y),
  mark_line()
)
save_spec("line/basic.json")

ggvis(df, props(x = ~x, y = ~y), transform_sort(),
  mark_line()
)
save_spec("line/sort.json")

ggvis(df, props(x = ~x, y = ~y), transform_sort(var = "y"),
  mark_line()
)
save_spec("line/sort-y.json")

ggvis(df, props(x = ~x, y = ~y, stroke = ~z), 
  branch_line()
)
save_spec("line/branch-line.json")
