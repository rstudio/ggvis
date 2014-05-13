library(ggvis)

# Histogram, fully specified
mtcars %>% ggvis(x = ~wt) %>%
  compute_bin(~wt, binwidth = 1) %>%
  layer_rects(x = ~xmin_, x2 = ~xmax_, y = ~count_, y2 = 0)

# Or using shorthand layer
mtcars %>% ggvis(x = ~wt) %>% layer_histograms()
mtcars %>% ggvis(x = ~wt) %>% layer_histograms(binwidth = 1)

# Histogram, filled by cyl
mtcars %>% ggvis(x = ~wt, fill = ~factor(cyl)) %>%
  group_by(cyl) %>%
  layer_histograms(binwidth = 1)

# Bigger dataset
data(diamonds, package = "ggplot2")
diamonds %>% ggvis(x = ~table) %>% layer_histograms()


# Stacked histogram
diamonds %>% ggvis(x = ~table, fill = ~cut) %>%
  group_by(cut) %>%
  compute_bin(~table, binwidth = 1) %>%
  compute_stack(~count_, ~x_) %>%
  layer_rects(x = ~xmin_, x2 = ~xmax_, y = ~stack_lwr_, y2 = ~stack_upr_,
              fillOpacity := 0.6)

# Histogram of dates
set.seed(2934)
dat <- data.frame(times = as.POSIXct("2013-07-01", tz = "GMT") + rnorm(200) * 60 * 60 * 24 * 7)
dat %>% ggvis(x = ~times) %>% layer_histograms()
