library(ggvis)
#simple summary brush tooltip
x_bar = "x&#772;"
sigma_hat = "&sigma;&#770;"
brushed_summary <- function(value, session) {
  if(is.null(value) || length(value) == 0 || length(value$items) == 0) {
    return(NULL)
  }
  # Looking at the first element only, get names other than key__
  names <- setdiff(names(value$items[[1]]), "key__")

  lines <- lapply(names, function(name) {
    vals <-  vapply(value$items, `[[`, name, FUN.VALUE= numeric(1))
    str <- paste0(name, ": ", x_bar, "=", round(mean(vals), 2), " ",
                  sigma_hat, "=", round(sd(vals), 2), "<BR>")
  })

  show_tooltip(session,
    pagex = value$pagex2 + 5,
    pagey = value$pagey1 + 5,
    html = lines
  )
}

# Scatter plot with brushing
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(size.brush := 400) %>%
  handle_brush(brushed_summary)

# Bar graph with brushing
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>%
  set_dscale("x", "nominal", range = "width", padding = 0, points = FALSE) %>%
  layer_rects(y2 = 0, width = band(), fill.brush := "red") %>%
  handle_brush(brushed_summary)

# Brushing with 10000 points
data("diamonds", package="ggplot2")
d <- diamonds[sample(nrow(diamonds), 10000), ]
d %>% ggvis(x = ~carat, y = ~price) %>%
  layer_points(size := 40, fillOpacity := 0.02, fillOpacity.brush := 0.4) %>%
  handle_brush(brushed_summary)
