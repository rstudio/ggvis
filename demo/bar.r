library(ggvis)

# Bar graph with continuous x
ggvis(pressure, props(x = ~temperature, y = ~pressure)) +
  mark_rect(props(y2 = 0, width := 15))

# Bar graph with ordinal x
ggvis(pressure, props(x = ~temperature, y = ~pressure)) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect(props(y2 = 0, width = band()))


# Hair and eye color data
library(plyr)
hec <- as.data.frame(HairEyeColor)
hec <- ddply(hec, c("Hair", "Eye"), summarise, Freq = sum(Freq))

# Without stacking - bars overlap
ggvis(hec, props(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5)) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect(props(y2 = 0, width = band()))

# With stacking
ggvis(hec, transform_stack(),
  props(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5)) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect(props(y = ~ymin__, y2 = ~ymax__, width = band()))

# Stacking in x direction instead of default y
ggvis(hec, transform_stack(direction = "x"),
  props(x = ~Freq, y = ~Hair, fill = ~Eye, fillOpacity := 0.5)) +
  dscale("y", "nominal", range = "height", padding = 0, points = FALSE) +
  mark_rect(props(x = ~xmin__, x2 = ~xmax__, height = band()))
