library(gigvis)

gigvis("mtcars", props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol()
)

gigvis("mtcars", props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  axes = list(axis("x", title = "Weight"))
)

gigvis("mtcars", props(x ~ wt, y ~ mpg, fill ~ cyl),
  mark_symbol(),
  legends = list(legend(fill = "fill", title = "Cylinders"))
)