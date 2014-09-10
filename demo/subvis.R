library(nasaweather)
library(ggvis)
library(dplyr)

# Compare to ggplot2 order is going to be important:
# in ggplot2 it didn't matter where the facetting specification appeared,
# it affected all datasets. But in ggvis, subvis only affects data after.
# This is probably easier for people to reason about.

nasa %>%
  group_by(lat, long) %>%
  ggvis(~lat, ~long) %>%
  subvis()

# Mostly eqiuvalent to
nasa %>%
  group_by(lat, long) %>%
  ggvis(~lat, ~long) %>%
  layer_rect()
# because a group mark is a rect mark with children

nasa %>%
  group_by(lat, long) %>%
  ggvis(~lat, ~long) %>%
  subvis() %>%
  layer_lines(~time, ~temp) # added to children of group

# Categorical top-level (facetting) --------------------------------------------
# Facetting corresponds to a top-level spec with two categorical scales
# It implicitly sets the width and height of each rect of band()

nasa %>%
  group_by(year, month) %>%
  ggvis(~year, ~month) %>%
  subvis() %>%
  layer_tile(~long, ~lat, fill = ~ozone)

# Or maybe:
nasa %>%
  facet_grid(~year, ~month) %>%
  layer_tile(~long, ~lat, fill = ~ozone)

# Facet wrap represents a transformation of the data
nasa %>%
  mutate(ymon = year + month / 12, row = rows(ymon), col = cols(ymon)) %>%
  group_by(row, col) %>%
  ggvis(~row, ~col) %>%
  ...

# Granular top-level -----------------------------------------------------------
# But really year and month aren't categorical - they're granular, as
# are lat and long. For granular data, the size of the rects can also be
# taken from the data: it's the resolution (but this must be set as x, x2,
# y and y2), like in layer_bars().

atmos %>%
  group_by(long, lat) %>%
  ggvis(~long, ~lat) %>%
  layer_group() %>%
  group_by(month) %>%
  layer_lines(~year, ~ozone)

# Or, subtly different
atmos %>%
  group_by(long, lat, month) %>%
  ggvis(~long, ~lat) %>%
  layer_group() %>%
  layer_lines(~year, ~ozone)
# We'd get one rect for each long, lat & month - the outputs would look
# basically the same because the default background for a group is transparent
# so the groups would overlap.

# Lat and long are interesting because there's another dataset we might
# want to display at the same time: a map of the countries in the area
borders %>%
  group_by(group) %>%
  ggvis(~long, ~lat) %>%
  layer_paths() %>%
  add_data(atmos %>% group_by(long, lat, month)) %>%
  layer_group() %>%
  layer_lines(~year, ~ozone)

# There's another dataset with values for each location: elev
atmos %>%
  group_by(long, lat, month) %>%
  left_join(elev, by = c("long", "lat")) %>%
  ggvis(~long, ~lat) %>%
  layer_rect(stroke = ~elev)

# Continuous top-level ---------------------------------------------------------
# Must supply width and height (in data units?)

# You might also want to display each location on some summarised position
atmos %>%
  group_by(lat, long) %>%
  summarise(surftemp = mean(surftemp), temp = mean(temp)) %>%
  ggvis(~surtemp, ~temp) %>%
  layer_lines() %>%
  left_join(atmos, by = c("lat", "long")) %>%
  #layer_group(width = ?, height = ?) %>%
  layer_points(~surftemp, ~temp)

# If grouped, then join needs to be subset of grouping variables.
# Controls which variables are groupwise constant and should be output
# at higher level - any variables added by join should be added to groups.

# Properties of layer_group must be groupwise constant. Groupwise data frames
# sent as csv, and then facetted on client side (like layer_paths).
# {"type": "facet", "keys": ["data.category"], ...}

# Isolation --------------------------------------------------------------------

# If you don't want subvis() to affect subsequent layers,
# use layer function to insulate:
nasa %>%
  group_by(lat, long) %>%
  ggvis(~lat, ~long) %>%
  layer(x %>% subvis() %>% layer_lines(~time, ~temp))

# This is most important when setting scales & axes.
# But it's also important if you want high-level data underneath low-level
# data (i.e. you want a map _on top of_ individual locations)
