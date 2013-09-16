library(ggvis)
library(shiny)

# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~ wt, y = ~ mpg),
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
ggvis(mtc1, props(x = ~ x, y = ~ y),
  mark_symbol(),
  dscale("x", "numeric", domain = c(0, 1))
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
ggvis(
  props(x = ~ wt, y = ~ mpg),
  branch(
    mtc1,
    mark_symbol(props(stroke := "black", fill := "black"))
  ),
  branch(
    mtc2,
    mark_symbol(props(fill := "red", size := 40))
  )
)

# With a transform
mtc1 <- reactive({
  invalidateLater(1000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~ wt, y = ~ mpg),
  mark_symbol(),
  branch_smooth()
)


# Data points moving from right to left
# (currently transitions aren't quite right)
set.seed(430)
dat <- data.frame(time = 1:10, value = rnorm(10))
ddat <- reactive({
  invalidateLater(1000, NULL);
  dat$time  <<- c(dat$time[-1], dat$time[length(dat$time)] + 1)
  dat$value <<- c(dat$value[-1], rnorm(1))
  dat
})
ggvis(ddat, props(x = ~time, y = ~value),
  mark_symbol(),
  mark_line()
)
