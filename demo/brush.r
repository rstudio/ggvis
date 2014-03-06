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
ggvis(mtcars, props(x = ~wt, y = ~mpg)) +
  layer_point(props(size.brush := 400)) +
  brush_tooltip(brushed_summary)

# Bar graph with brushing
ggvis(pressure, props(x = ~temperature, y = ~pressure)) +
  dscale("x", "nominal", range = "width", padding = 0, points = FALSE) +
  mark_rect(props(y2 = 0, width = band(), fill.brush := "red")) +
  brush_tooltip(brushed_summary)

# Brushing with 10000 points
data("diamonds", package="ggplot2")
d <- diamonds[sample(nrow(diamonds), 10000), ]
ggvis(d, props(x = ~carat, y = ~price)) +
  layer_point(props(size := 40, fillOpacity := 0.02, fillOpacity.brush := 0.4)) +
  brush_tooltip(brushed_summary)
