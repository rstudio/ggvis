# Modifying scales
library(ggvis)

ggvis(mtcars, props(x = ~wt),
  layer_histogram(binwidth = 1)
)
save_spec("layer/histogram.json")

ggvis(mtcars, props(x = ~wt, stroke = ~cyl), by_group(cyl),
  layer_freqpoly(binwidth = 1)
)
save_spec("layer/freqpoly-grouped.json")

ggvis(mtcars, props(x = ~wt, y = ~mpg),
  layer_smooth(method = "loess", formula = y ~ x)
)
save_spec("layer/smooth.json")

ggvis(mtcars, props(x = ~wt, y = ~mpg), by_group(cyl),
  layer_smooth(method = "lm", formula = y ~ x)
)
save_spec("layer/smooth-grouped.json")
