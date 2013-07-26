library(gigvis)
library(shiny)

# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
gigvis(mtc1, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  dynamic = TRUE
)


# Two separate data sets, equal in the tree
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
mtc2 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
gigvis(data = NULL, props = props(x ~ wt, y ~ mpg),
  node(
    data = mtc1,
    mark_symbol(stroke = "black", fill = "black")
  ),
  node(
    data = mtc2,
    mark_symbol(fill = "red", size = 40)
  ),
  dynamic = TRUE
)

# With a transform
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
gigvis(mtc1, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  node(
    data = transform_smooth(method = "lm"),
    mark_line(stroke = "red")
  ),
  dynamic = TRUE
)
