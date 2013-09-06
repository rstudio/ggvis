# Modifying scales
library(ggvis)

ggvis(mtcars, props(x = ~wt),
  branch_histogram(binwidth = 1)
)
save_spec("branch/histogram.json")

ggvis(mtcars, props(x = ~wt, stroke = ~cyl), by_group(cyl),
  branch_freqpoly(binwidth = 1)
)
save_spec("branch/freqpoly-grouped.json")

ggvis(mtcars, props(x = ~wt, y = ~mpg),
  branch_smooth(method = "loess", formula = y ~ x)
)
save_spec("branch/smooth.json")

ggvis(mtcars, props(x = ~wt, y = ~mpg), by_group(cyl),
  branch_smooth(method = "lm", formula = y ~ x)
)
save_spec("branch/smooth-grouped.json")
