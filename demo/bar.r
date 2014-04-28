library(ggvis)

# Bar graph with continuous x, and bars 15 pixels wide
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  mark_rect(props(y2 = 0, width := 15))

# Bar graph with continuous x, and bars occupying full width
pressure %>% ggvis(x = ~temperature + 10, x2 = ~temperature - 10,
                   y = ~pressure, y2 = 0) %>%
  mark_rect()

# Bar graph with categorical x
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  set_dscale("x", "nominal", padding = 0, points = FALSE) %>%
  mark_rect(props(y2 = 0, width = band()))


# Hair and eye color data
hec <- as.data.frame(HairEyeColor)
hec <- plyr::ddply(hec, c("Hair", "Eye"), summarise, Freq = sum(Freq))

# Without stacking - bars overlap
hec %>% ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  set_dscale("x", "nominal", range = "width", padding = 0, points = FALSE) %>%
  mark_rect(props(y2 = 0, width = band()))

# With stacking
hec %>% ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  transform_stack() %>%
  set_dscale("x", "nominal", range = "width", padding = 0, points = FALSE) %>%
  mark_rect(props(y = ~ymin__, y2 = ~ymax__, width = band()))

# Stacking in x direction instead of default y
hec %>% ggvis(x = ~Freq, y = ~Hair, fill = ~Eye, fillOpacity := 0.5) %>%
  transform_stack(direction = "x") %>%
  set_dscale("y", "nominal", range = "height", padding = 0, points = FALSE) %>%
  mark_rect(props(x = ~xmin__, x2 = ~xmax__, height = band()))
