library(ggvis)

# Bar graph with continuous x, and bars 15 pixels wide
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  layer_rects(y2 = 0, width := 15)

# Bar graph with continuous x, and bars occupying full width
pressure %>%
  ggvis(x = ~temperature + 10, x2 = ~temperature - 10,
        y = ~pressure, y2 = 0) %>%
  layer_rects()

# Bar graph with categorical x
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  set_dscale("x", "nominal", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band())


# Hair and eye color data
hec <- as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))

# Without stacking - bars overlap
hec %>% ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  set_dscale("x", "nominal", range = "width", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band())

# With stacking
hec %>% ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  compute_stack() %>%
  set_dscale("x", "nominal", range = "width", padding = 0, points = FALSE) %>%
  layer_rects(y = ~stack_lwr_, y2 = ~stack_upr_, width = band())

# Stacking in x direction instead of default y
hec %>% ggvis(x = ~Freq, y = ~Hair, fill = ~Eye, fillOpacity := 0.5) %>%
  compute_stack(stack_var = ~Freq, group_var = ~Hair) %>%
  set_dscale("y", "nominal", range = "height", padding = 0, points = FALSE) %>%
  layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band())
