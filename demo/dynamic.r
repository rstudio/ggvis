library(ggvis)
library(shiny)

# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~wt, y = ~mpg)) + layer_point()

# Rapidly changing dynamic example
mtc1 <- reactive({
  invalidateLater(200, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~wt, y = ~mpg)) + layer_point()
df <- data.frame(x = runif(20), y = runif(20))

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
  layer(mtc1, layer_point(props(stroke := "black", fill := "black"))) +
  layer(mtc2, layer_point(props(fill := "red", size := 40)))

# With a transform
mtc1 <- reactive({
  invalidateLater(1000, NULL)
  mtcars[sample(nrow(mtcars), 10), ]
})
ggvis(mtc1, props(x = ~wt, y = ~mpg)) +
  layer_point() +
  layer_smooth()

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
  layer_point() +
  mark_path()


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
