library(ggvis)

ggvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol()
)

ggvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  axis("x", title = "Weight")
)

ggvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  legend(fill = "fill", title = "Cylinders")
)
