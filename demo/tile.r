library(ggvis)

pp <- function (n, r = 4) {
  width <- 2 * r * pi / (n - 1)

  mid <- seq(-r * pi, r * pi, len = n)
  df <- expand.grid(x = mid, y = mid)
  df$r <- sqrt(df$x^2 + df$y^2)
  df$z <- cos(df$r^2)*exp(-df$r/6)

  df$y2 <- df$y + width
  df$x2 <- df$x + width
  df$x <- df$x - width
  df$y <- df$y - width
  df
}

pp(100) %>% ggvis(~x, ~y, x2 = ~x2, y2 = ~y2, fill = ~ z, stroke := NA) %>%
  layer_rects()
