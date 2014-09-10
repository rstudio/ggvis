context("compute_count")

test_that("count_vector correctly handles factors", {
  fac <- factor(c("A", "A", "C"), levels = c("A", "C"))
  expect_identical(
    count_vector(fac),
    data.frame(
      count_ = c(2, 1),
      x_ = factor(c("A", "C"), levels = c("A", "C"))
    )
  )

  # Levels that aren't represented are dropped from the count
  fac <- factor(c("A", "A", "C"), levels = c("A", "B", "C"))
  expect_identical(
    count_vector(fac),
    data.frame(
      count_ = c(2, 1),
      x_ = factor(c("A", "C"), levels = c("A", "B", "C"))
    )
  )

  # Factor with levels in non-lexical order
  fac <- factor(c("A", "A", "C"), levels = c("C", "A"))
  expect_identical(
    count_vector(fac),
    data.frame(
      count_ = c(1, 2),
      x_ = factor(c("C", "A"), levels = c("C", "A"))
    )
  )
})

test_that("count_vector preserves dates and times", {
  dates <- as.Date("2013-07-01") + 1:100
  res <- count_vector(dates)
  # May not be identical because res$x_ gets coerced to int
  expect_equal(dates, res$x_)

  times <- as.POSIXct('2001-06-11 21:00', tz = 'America/New_York') + 1:10 * 100
  res <- count_vector(times)
  expect_identical(times, res$x_)

  times <- as.POSIXct('2001-06-11 21:00', tz = 'UTC') + seq(1, 1000, by = 10)
  res <- count_vector(times)
  expect_identical(times, res$x_)
})
