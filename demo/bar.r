library(ggvis)

# Bar graph with continuous x, and y value supplied in the data
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  layer_bars()

# Bar graph with continuous x, and y value supplied. Although the x value is
# continuous, a categorical scale is used here.
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  set_dscale("x", "nominal", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band(mult = 0.9))

# Categorical x, and y var supplied
# FIXME: x values are currently sorted alphabetically, instead of by factor
# level order.
pressure %>% ggvis(~factor(temperature), ~pressure) %>% layer_bars()

# No y var, and continuous x: bar graph of counts
mtcars %>% ggvis(x = ~cyl) %>% layer_bars()

# Notice how it differs from a histogram: a histogram has bins that span
# ranges of x, but layer_bars shows the count at each unique x value.
mtcars %>% ggvis(~wt) %>% layer_histograms()
mtcars %>% ggvis(~wt) %>% layer_bars()

# No y var, and discrete x: bar graph of counts at each x value
mtcars %>% ggvis(~factor(cyl)) %>% layer_bars()


# Hair and eye color data
hec <- as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))

# Without stacking - bars overlap
hec %>% group_by(Eye) %>%
  ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  layer_bars(stack = FALSE)

# With stacking
hec %>% group_by(Eye) %>%
  ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  layer_bars()

# Stacking in x direction instead of default y - need to be explicit about
# all the steps
hec %>% group_by(Eye) %>%
  ggvis(y = ~Hair, fill = ~Eye, fillOpacity := 0.5) %>%
  compute_stack(stack_var = ~Freq, group_var = ~Hair) %>%
  layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
  set_dscale("y", "nominal", range = "height", padding = 0, points = FALSE)
