# Basic scatterplot
library(ggvis)

set.seed(1014)
df <- data.frame(x = 1:5, y = 5:1, z = runif(5))

ggvis(df, props(x = ~x, y = ~x),
  layer_point()
)
save_spec("scatter/basic.json")

ggvis(df, props(x = ~factor(x), y = ~x / y),
  layer_point()
)
save_spec("scatter/transform.json")

ggvis(df, props(x = ~x, y = ~y, fill = ~z),
  layer_point()
)
save_spec("scatter/fill-continuous.json")

ggvis(df, props(x = ~x, y = ~y, fill = ~factor(z)),
  layer_point()
)
save_spec("scatter/fill-discrete.json")
