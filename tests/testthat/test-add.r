context("add")

# Given a named list, return a named list with items sorted by name
nsort <- function(x) x[sort(names(x))]


test_that("Adding to ggvis objects", {
  # In each test, we add two of the same type of object because this adds when
  # there's no existing object of the type, and then adds when there is an
  # existing object.

  # Marks
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), mark_path())
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg))
  p2 <- p2 + mark_symbol()
  p2 <- p2 + mark_path()
  expect_equal(nsort(p1), nsort(p2))

  # Layers
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), layer_smooth())
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg))
  p2 <- p2 + mark_symbol()
  p2 <- p2 + layer_smooth()
  expect_equal(nsort(p1), nsort(p2))

  # Scales
  p1 <- ggvis(mtcars,
    props(x = ~wt, y = ~mpg, fill = ~factor(cyl), fillOpacity = ~hp),
    mark_symbol(),
    dscale("opacity", "numeric", range = c(0.2, 1)),
    dscale("x", "numeric", domain = c(1, 10))
  )
  p2 <- ggvis(mtcars,
    props(x = ~wt, y = ~mpg, fill = ~factor(cyl), fillOpacity = ~hp),
    mark_symbol()
  )
  p2 <- p2 + dscale("opacity", "numeric", range = c(0.2, 1))
  p2 <- p2 + dscale("x", "numeric", domain = c(1, 10))
  expect_equal(nsort(p1), nsort(p2))

  # Legends
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol(),
    guide_legend(fill = "fill", title = "Cylinders")
  )
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol()
  )
  p2 <- p2 + guide_legend(fill = "fill", title = "Cylinders")
  expect_equal(nsort(p1), nsort(p2))

  # Axes
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol(),
    guide_axis("x", title = "Weight"),
    guide_axis("y", title = "Miles per gallon")
  )
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol()
  )
  p2 <- p2 + guide_axis("x", title = "Weight")
  p2 <- p2 + guide_axis("y", title = "Miles per gallon")
  expect_equal(nsort(p1), nsort(p2))

  # Props
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol()
  )
  p2 <- ggvis(mtcars, mark_symbol())
  p2 <- p2 + props(x = ~wt, y = ~mpg)
  p2 <- p2 + props(fill = ~cyl)
  expect_equal(nsort(p1), nsort(p2))

  # Opts
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
              opts(width = 200, height = 100))

  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol())
  p2 <- p2 + opts(width = 200)
  p2 <- p2 + opts(height = 100)

  expect_equal(nsort(p1), nsort(p2))
})


test_that("Adding to layer objects", {
  # Marks
  expect_equal(
    layer() + mark_symbol() + mark_path(),
    layer(mark_symbol(), mark_path())
  )

  # Props
  expect_equal(
    nsort(layer() + props(x = ~wt) + props(y = ~mpg)),
    nsort(layer(props(x = ~wt), props(y = ~mpg)))
  )

  # Marks and props
  expect_equal(
    nsort(layer() + props(x = ~wt) + mark_path()),
    nsort(layer(props(x = ~wt), mark_path()))
  )

  expect_error(layer() + dscale("x", "numeric"))
  expect_error(layer() + guide_axis("x"))
  expect_error(layer() + guide_legend("fill"))
  expect_error(layer() + opts())
})


test_that("Adding to mark objects", {
  # Props
  expect_equal(
    nsort(mark_symbol() + props(x = ~wt) + props(y = ~mpg)),
    nsort(mark_symbol(props(x = ~wt, y = ~mpg)))
  )

  expect_error(mark_path() + mark_rect())
  expect_error(mark_symbol() + dscale("x", "numeric"))
  expect_error(mark_symbol() + guide_axis("x"))
  expect_error(mark_path() + guide_legend("fill"))
  expect_error(mark_symbol() + opts())
})
