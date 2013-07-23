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
  p <- pipeline(mtcars, by_group(variable(quote(cyl))))
  dat <- flow(p, prop)
  expect_equal(length(dat), 3)
  expect_true(has_unique_combinations(dat, "cyl"))

  # Splitting by two variables, in one by_group()
  p <- pipeline(mtcars,
                by_group(variable(quote(cyl)),
                         variable(quote(am))))
  dat <- flow(p, prop)
  expect_equal(length(dat), 6)
  expect_true(has_unique_combinations(dat, "cyl", "am"))

  # Splitting by two variables, in two separate by_group()
  p <- pipeline(mtcars,
                by_group(variable(quote(cyl))),
                by_group(variable(quote(am))))
  dat <- flow(p, prop)
  expect_equal(length(dat), 6)
  expect_true(has_unique_combinations(dat, "cyl", "am"))
})
