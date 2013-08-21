context("split")

test_that("splitting by group", {
  # Helper function: Given a split_df, return TRUE if each piece has a unique
  # combination of the of the columns named by ...
  has_unique_combinations <- function(x, ...) {
    # For each data frame in dat...
    res <- lapply(x, function(dat) {
      # Get the unique combinations of values for the vars
      unique_vals <- lapply(list(...), function(var) unique(dat[[var]]))
      interaction(unique_vals)
    })

    # Check each data frame had one pairing
    if (any(vapply(res, length, numeric(1)) != 1))
      return(FALSE)

    # Check that no combination is duplicated
    res <- unlist(res)
    if (length(res) != length(levels(res)))
      return(FALSE)

    TRUE
  }

  # Properties; will be used repeatedly
  prop <- props(x ~ wt, y ~ mpg, stroke ~ cyl)


  # Splitting by one variable
  p <- pipeline(mtcars, by_group(quote(cyl)))
  dat <- sluice(p, prop)
  expect_equal(length(dat), 3)
  expect_true(has_unique_combinations(dat, "cyl"))

  # Splitting by two variables, in one by_group().
  # Also use strings to specify group vars
  p <- pipeline(mtcars, by_group("cyl", "am"))
  dat <- sluice(p, prop)
  expect_equal(length(dat), 6)
  expect_true(has_unique_combinations(dat, "cyl", "am"))

  # Splitting by two variables, in two separate by_group()
  p <- pipeline(mtcars, by_group("cyl"), by_group("am"))
  dat <- sluice(p, prop)
  expect_equal(length(dat), 6)
  expect_true(has_unique_combinations(dat, "cyl", "am"))
})

test_that("by_group accepts strings or quoted expressions", {
  q <- by_group(quote(cyl))
  s <- by_group("cyl")
  expect_identical(q, s)

  qq <- by_group(quote(cyl), quote(am))
  qs <- by_group(quote(cyl), "am")
  ss <- by_group("cyl", "am")
  expect_identical(qq, qs)
  expect_identical(qs, ss)

  # A quoted expression is not the same as a weird variable name
  q <- by_group(quote(cyl/2))
  s <- by_group("cyl/2")
  expect_false(identical(q, s))

  # Two ways of picking out weird variable names
  q <- by_group(quote(`cyl/2`))
  s <- by_group("cyl/2")
  expect_identical(q, s)
})

test_that("sluicing by_group uses environment for evaluation", {
  sp <- function(data, group) {
    sluice(pipeline(data, group), props(x ~ wt, y ~ mpg))
  }

  x <- sp(mtcars, by_group("cyl"))
  expect_equal(length(x), 3)

  # Get variable from calling environment
  foo <- rep(1:4, nrow(mtcars)/4)
  x <- sp(mtcars, by_group("foo"))
  expect_equal(length(x), 4)

  # Get variable from a specified environment
  e <- new.env()
  e$foo <- rep(1:2, nrow(mtcars)/2)
  x <- sp(mtcars, by_group("foo", env = e))
  expect_equal(length(x), 2)

  # Evaluate quoted expression
  x <- sp(mtcars, by_group(quote(ceiling(foo/2))))
  expect_equal(length(x), 2)

  # Strings aren't expressions; they refer to names. In this case, it's a
  # weird var name in the data
  mtc <- mtcars
  mtc[["ceiling(foo/2)"]] <- 1
  x <- sp(mtc, by_group("ceiling(foo/2)"))
  expect_equal(length(x), 1)

  # Another weird var name, from the calling environment
  `ceiling(foo/4)` <- 10
  x <- sp(mtc, by_group("ceiling(foo/4)"))
  expect_equal(length(x), 1)
})
