library(ggvis)
library(shiny)

# Basic dynamic example
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
mtc1 %>% ggvis(x = ~wt, y = ~mpg) %>% layer_points()

# Rapidly changing dynamic example
df <- data.frame(x = runif(20), y = runif(20))
mtc1 <- reactive({
  invalidateLater(200, NULL);

  df$x <<- df$x + runif(20, -0.05, 0.05)
  df$y <<- df$y + runif(20, -0.05, 0.05)
  df
})
mtc1 %>% ggvis(x = ~x, y = ~y) %>%
  layer_points() %>%
  scale_numeric("x", domain = c(0, 1), clamp = TRUE) %>%
  scale_numeric("y", domain = c(0, 1), clamp = TRUE) %>%
  set_options(duration = 0)

# Two separate data sets, equal in the tree
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
mtc2 <- reactive({
  invalidateLater(1000, NULL);
  mtcars[sample(nrow(mtcars), 10), ]
})
NULL %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(data = mtc1, stroke := "black", fill := "black") %>%
  layer_points(data = mtc2, fill := "red", size := 40)

# With a transform
mtc1 <- reactive({
  invalidateLater(1000, NULL)
  mtcars[sample(nrow(mtcars), 10), ]
})
mtc1 %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_smooths()

# Data points moving from right to left
set.seed(430)
dat <- data.frame(time = 1:10, value = runif(10))
ddat <- reactive({
  invalidateLater(2000, NULL)
  dat$time  <<- c(dat$time[-1], dat$time[length(dat$time)] + 1)
  dat$value <<- c(dat$value[-1], runif(1))
  dat
})
ddat %>% ggvis(x = ~time, y = ~value, key := ~time) %>%
  layer_points() %>%
  layer_paths()

# Bars moving from right to left
set.seed(430)
dat <- data.frame(time = 1:10, value = runif(10))
ddat <- reactive({
  invalidateLater(2000, NULL);
  dat$time  <<- c(dat$time[-1], dat$time[length(dat$time)] + 1)
  dat$value <<- c(dat$value[-1], runif(1))
  dat
})
ddat %>%
  ggvis(
    x = ~time, x.enter = ~time + 1, x.exit  = ~time - 1,
    y = ~value, y.enter = 0, y.exit = 0,
    y2 = 0, y2.enter = 0, y2.exit = 0,
    fill := "#aaa",
    fillOpacity := 1, fillOpacity.enter := 0, fillOpacity.exit := 0,
    strokeOpacity := 1, strokeOpacity.enter := 0, strokeOpacity.exit := 0,
    width = band(),
    key := ~time
  ) %>%
  scale_numeric("y", domain = 0:1) %>%
  scale_nominal("x", domain = reactive(unique(ddat()$time)), padding = 0,
                points = FALSE) %>%
  layer_rects()


# Dynamic stacked bars
dat <- data.frame(
  g1 = rep(letters[1:4], 3),
  g2 = rep(LETTERS[1:3], each = 4),
  value = runif(12)
)
ddat <- reactive({
  invalidateLater(2000, NULL)
  dat$value <<- runif(12)
  dat
})
ddat %>% ggvis(x = ~g1, y = ~value, fill = ~g2, fillOpacity := 0.5) %>%
  layer_bars()
