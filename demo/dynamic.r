library(ggvis)
library(shiny)

# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~wt, y = ~mpg)) + mark_symbol()

# Rapidly changing dynamic example
df <- data.frame(x = runif(20), y = runif(20))
# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(20, NULL);

  df$x <<- df$x + runif(20, -0.05, 0.05)
  df$y <<- df$y + runif(20, -0.05, 0.05)
  df
})
ggvis(mtc1, props(x = ~x, y = ~y)) +
  mark_symbol() +
  dscale("x", "numeric", domain = c(0, 1))

# Two separate data sets, equal in the tree
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
mtc2 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(props(x = ~wt, y = ~mpg)) +
  branch(mtc1, mark_symbol(props(stroke := "black", fill := "black"))) +
  branch(mtc2, mark_symbol(props(fill := "red", size := 40)))

# With a transform
mtc1 <- reactive({
  invalidateLater(1000, NULL)
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~wt, y = ~mpg)) +
  mark_symbol() +
  branch_smooth()


# Data points moving from right to left
# (currently transitions aren't quite right)
set.seed(430)
dat <- data.frame(time = 1:10, value = runif(10))
ddat <- reactive({
  invalidateLater(2000, NULL)
  dat$time  <<- c(dat$time[-1], dat$time[length(dat$time)] + 1)
  dat$value <<- c(dat$value[-1], runif(1))
  dat
})
ggvis(ddat, props(x = ~time, y = ~value, key := ~time)) +
  mark_symbol() +
  mark_line()


# Bars moving from right to left
set.seed(430)
dat <- data.frame(time = 1:10, value = runif(10))
ddat <- reactive({
  invalidateLater(2000, NULL);
  dat$time  <<- c(dat$time[-1], dat$time[length(dat$time)] + 1)
  dat$value <<- c(dat$value[-1], runif(1))
  dat
})
ggvis(ddat, props(
    x = ~time, x.enter = ~time + 1, x.exit  = ~time - 1,
    y = ~value, y.enter = 0, y.exit = 0,
    y2 = 0, y2.enter = 0, y2.exit = 0,
    fill := "#aaa",
    fillOpacity := 1, fillOpacity.enter := 0, fillOpacity.exit := 0,
    strokeOpacity := 1, strokeOpacity.enter := 0, strokeOpacity.exit := 0,
    width = band(),
    key := ~time
  )) +
  dscale("y", "numeric", domain = 0:1) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect()


# Dynamic stacked bars
dat <- data.frame(
  g1 = rep(letters[1:4], 3),
  g2 = rep(LETTERS[1:3], each = 4),
  value = runif(12)
)
ddat <- reactive({
  invalidateLater(3000, NULL)
  dat$value <<- runif(12)
  dat
})
ggvis(ddat, transform_stack(),
  props(x = ~g1, y = ~value, fill = ~g2, fillOpacity := 0.5)) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect(props(y = ~ymin__, y2 = ~ymax__, width = band()))
