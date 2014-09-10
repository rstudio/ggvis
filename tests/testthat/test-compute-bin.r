context("compute_bin")

test_that("bin_vector preserves dates and times", {
  dates <- as.Date("2013-07-01") + 1:100
  res <- bin_vector(dates, binwidth = 30)
  expect_true(inherits(res$x_, "Date"))
  expect_true(inherits(res$xmin_, "Date"))
  expect_true(inherits(res$xmax_, "Date"))
  expect_identical(sum(res$count_), 100)

  times <- as.POSIXct('2001-06-11 21:00', tz = 'America/New_York') + 1:10 * 100
  res <- bin_vector(times, binwidth = 120)
  expect_true(inherits(res$x_, "POSIXct"))
  expect_true(inherits(res$xmin_, "POSIXct"))
  expect_true(inherits(res$xmax_, "POSIXct"))
  expect_identical(sum(res$count_), 10)
  expect_identical(attr(times, "tzone"), attr(res$x_, "tzone"))

  times <- as.POSIXct('2001-06-11 21:00', tz = 'UTC') + seq(1, 1000, by = 10)
  res <- bin_vector(times, binwidth = 120)
  expect_identical(sum(res$count_), 100)
  expect_identical(attr(times, "tzone"), attr(res$x_, "tzone"))


  # Can set boundary
  dates <- as.Date("2013-06-01") + 1:100
  res <- bin_vector(dates, binwidth = 30, boundary = as.Date("2013-06-01"),
                    pad = FALSE)
  expect_identical(sum(res$count_), 100)
  expect_identical(res$xmin_[1], as.Date("2013-06-01"))

  res <- bin_vector(times, binwidth = 120,
                    boundary = as.POSIXct('2001-06-11 21:00', tz = 'UTC'),
                    pad = FALSE)
  expect_identical(sum(res$count_), 100)
  expect_identical(res$xmin_[2], as.POSIXct('2001-06-11 21:00', tz = 'UTC'))
})
