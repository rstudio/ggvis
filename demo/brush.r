library(ggvis)
#simple summary brush tooltip
x_bar <- "x&#772;"
sigma_hat <- "&sigma;&#770;"

brushed_summary <- function(items, session, page_loc, ...) {
  if (nrow(items) == 0) return()

  items$key__ <- NULL
  lines <- Map(function(name, vals) {
    paste0(name, ": ",
      x_bar, " = ", round(mean(vals), 2), " ",
      sigma_hat, " = ", round(sd(vals), 2)
    )
  }, names(items), items)
  html <- paste(unlist(lines), collapse = "<br />\n")

  show_tooltip(session, page_loc$r + 5, page_loc$t, html)
}

# Scatter plot with brushing
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(size.brush := 400) %>%
  handle_brush(brushed_summary)

# Bar graph with brushing
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  scale_nominal("x", range = "width", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band(), fill.brush := "red") %>%
  handle_brush(brushed_summary)

# Brushing with 10000 points
data("diamonds", package="ggplot2")
d <- diamonds[sample(nrow(diamonds), 10000), ]
d %>% ggvis(x = ~carat, y = ~price) %>%
  layer_points(size := 40, fillOpacity := 0.02, fillOpacity.brush := 0.4) %>%
  handle_brush(brushed_summary)
