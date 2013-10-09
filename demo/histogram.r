library(ggvis)

# Histogram, fully specified
ggvis(
  pipeline(mtcars, transform_bin(binwidth = 1)),
  props(x = ~wt),
  branch(
    props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0),
    mark_rect()
  )
)

# Or using shorthand branch
ggvis(mtcars, props(x = ~wt),
  branch_histogram(binwidth = 1)
)
ggvis(mtcars, props(x = ~wt),
  branch_histogram()
)

# Histogram, filled by cyl
by_cyl <- pipeline(mtcars, by_group(cyl))
ggvis(by_cyl, props(x = ~wt, fill = ~factor(cyl)),
  branch_histogram(binwidth = 1))

ggvis(by_cyl, props(x = ~wt, stroke = ~factor(cyl)),
  branch_freqpoly(binwidth = 1))


# Bigger dataset
data(diamonds, package = "ggplot2")
ggvis(diamonds, props(x = ~table),
  branch_histogram()
)


# Stacked histogram
ggvis(
  mtcars, by_group(cyl), props(x = ~wt, y = ~mpg, fill = ~factor(cyl)),
  branch(
    transform_bin(binwidth = 0.25),
    branch(
      props(x = ~xmin__, x2 = ~xmax__, y = ~count__, fillOpacity := 0.6),
      branch(
        transform_stack(),
        mark_rect(props(y = ~y_upper__, y2 = ~y_lower__))
      )
    )
  )
)
