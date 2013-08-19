library(ggvis)

ggvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol()
)

ggvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  guide_axis("x", title = "Weight")
)

ggvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  guide_legend(fill = "fill", title = "Cylinders")
)
