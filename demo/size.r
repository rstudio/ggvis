library(ggvis)

# Set size to 300x300 pixels
ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), size(300, 300))

# Set size to 300x300 pixels, and add 50 pixels padding on all sides
ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
  size(300, 300), padding(50, 50, 50, 50))
