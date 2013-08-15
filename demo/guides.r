library(gigvis)

gigvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol()
)

gigvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  axis("x", title = "Weight")
)

gigvis(mtcars, props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  legend(fill = "fill", title = "Cylinders")
)
