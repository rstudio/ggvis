context("add")

# Given a named list, return a named list with items sorted by name
nsort <- function(x) x[sort(names(x))]


test_that("Adding marks and branches", {
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), mark_line())
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg))
  p2 <- p2 + mark_symbol()
  p2 <- p2 + mark_line()
  expect_equal(nsort(p1), nsort(p2))

  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(), branch_smooth())
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg))
  p2 <- p2 + mark_symbol()
  p2 <- p2 + branch_smooth()
  expect_equal(nsort(p1), nsort(p2))
})


test_that("Adding scales", {
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
})


test_that("Adding legends", {
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol(),
    guide_legend(fill = "fill", title = "Cylinders")
  )
  
  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol()
  )
  p2 <- p2 + guide_legend(fill = "fill", title = "Cylinders")

  expect_equal(nsort(p1), nsort(p2))
})


test_that("Adding axes", {
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
})


test_that("Adding props", {
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg, fill = ~cyl),
    mark_symbol()
  )
  
  p2 <- ggvis(mtcars, mark_symbol())
  p2 <- p2 + props(x = ~wt, y = ~mpg)
  p2 <- p2 + props(fill = ~cyl)

  expect_equal(nsort(p1), nsort(p2))
})


test_that("Adding opts", {
  p1 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
              opts(width = 200, height = 100))

  p2 <- ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol())
  p2 <- p2 + opts(width = 200)
  p2 <- p2 + opts(height = 100)

  expect_equal(nsort(p1), nsort(p2))
})
