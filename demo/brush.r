library(ggvis)
#simple summary brush tooltip
x_bar = "x&#772;"
sigma_hat = "&sigma;&#770;"
brushed_summary <- function(x) {
  if(is.null(x) || length(x) == 0) return(NULL)
  names <- setdiff(names(x[[1]]), "key__")

  lines <- lapply(names, function(name) {
                  vals = vapply(x, `[[`, name, FUN.VALUE= numeric(1))
                  paste0(name, ": ", x_bar, "=", round(mean(vals), 2)," ",
                         sigma_hat, "=", round(sd(vals), 2), "<BR>")
  })
}
# Scatter plot with brushing
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(size.brush := 400) %>%
  add_brush_tooltip(brushed_summary)

# Bar graph with brushing
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  set_dscale("x", "nominal", range = "width", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band(), fill.brush := "red") %>%
  add_brush_tooltip(brushed_summary)

# Brushing with 10000 points
data("diamonds", package="ggplot2")
d <- diamonds[sample(nrow(diamonds), 10000), ]
d %>% ggvis(x = ~carat, y = ~price) %>%
  layer_points(size := 40, fillOpacity := 0.02, fillOpacity.brush := 0.4) %>%
  add_brush_tooltip(brushed_summary)
