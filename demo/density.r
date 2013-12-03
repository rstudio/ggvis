library(ggvis)

# Basic
ggvis(faithful, props(x = ~waiting)) + branch_density()

# Smaller bandwidth
ggvis(faithful, props(x = ~waiting, fill := "lightblue")) +
  branch_density(adjust = .25)

# Control stroke and fill
ggvis(faithful, props(x = ~waiting)) +
  branch_density(props(stroke := "#cc3333", strokeWidth := 3,
    fill := "#666699", fillOpacity := 0.6))

# With groups
ggvis(PlantGrowth, by_group(group),
  props(x = ~weight, stroke = ~group, fill = ~group, fillOpacity := 0.2)) +
  branch_density()

# Various arguments: adjust na.rm, n, area, kernel
mtc <- mtcars
mtc$mpg[5:10] <- NA
ggvis(mtc, props(x = ~mpg, y = ~mpg)) +
  branch_density(adjust = 0.3, n = 100, area = FALSE, kernel = "rectangular",
    props(stroke := "#cc0000"))
