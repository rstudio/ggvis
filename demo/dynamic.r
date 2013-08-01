library(gigvis)
library(shiny)

# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
gigvis(mtc1, props(x ~ wt, y ~ mpg),
  mark_symbol()
)

# Rapidly changing dynamic example
df <- data.frame(x = runif(20), y = runif(20))
# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(20, NULL);
  
  df$x <<- df$x + runif(20, -0.05, 0.05)
  df$y <<- df$y + runif(20, -0.05, 0.05)
  df
})
gigvis(mtc1, props(x ~ x, y ~ y),
  mark_symbol(),
  scales = scales(x = scale("x", domain = c(0, 1)))
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
    mark_symbol(props(stroke = "black", fill = "black"))
  ),
  node(
    data = mtc2,
    mark_symbol(props(fill = "red", size = 40))
  )
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
    mark_line(props(stroke = "red"))
  )
)
