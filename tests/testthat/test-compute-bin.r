context("compute_bin")

test_that("bin_vector preserves dates and times", {
  dates <- as.Date("2013-06-01") + 0:100
  UTCtimes <- as.POSIXct('2001-06-01 21:00', tz = 'UTC') + seq(0, 1000, by = 10)
  NYtimes <- as.POSIXct('2001-06-01 21:00', tz = 'America/New_York') + 0:10 * 100

  res <- bin_vector(dates, width = 30)
  expect_true(inherits(res$x_, "Date"))
  expect_true(inherits(res$xmin_, "Date"))
  expect_true(inherits(res$xmax_, "Date"))
  expect_identical(sum(res$count_), length(dates))

  res <- bin_vector(NYtimes, width = 120)
  expect_true(inherits(res$x_, "POSIXct"))
  expect_true(inherits(res$xmin_, "POSIXct"))
  expect_true(inherits(res$xmax_, "POSIXct"))
  expect_identical(sum(res$count_), length(NYtimes))
  expect_identical(attr(NYtimes, "tzone"), attr(res$x_, "tzone"))

  res <- bin_vector(UTCtimes, width = 120)
  expect_identical(sum(res$count_), length(UTCtimes))
  expect_identical(attr(UTCtimes, "tzone"), attr(res$x_, "tzone"))


  # Can set boundary
  res <- bin_vector(dates, width = 30, boundary = as.Date("2013-06-01"), pad = FALSE)
  expect_identical(sum(res$count_), length(dates))
  expect_identical(res$xmin_[1], as.Date("2013-06-01"))

  res <- bin_vector(UTCtimes, width = 120,
                    boundary = as.POSIXct('2001-06-01 21:07', tz = 'UTC'),
                    pad = FALSE)
  expect_identical(sum(res$count_), length(UTCtimes))
  expect_identical(res$xmin_[5], as.POSIXct('2001-06-01 21:07', tz = 'UTC'))

  # Can set center
  res <- bin_vector(dates, width = 30, center=as.Date("2013-07-01"),
                    pad = FALSE)
  expect_identical(sum(res$count_), length(dates))
  expect_identical(res$x_[2], as.Date("2013-07-01"))

  res <- bin_vector(UTCtimes, width = 120,
                    center = as.POSIXct('2001-06-01 21:15', tz = 'UTC'),
                    pad = FALSE)
  expect_identical(sum(res$count_), length(UTCtimes))
  expect_identical(res$x_[8], as.POSIXct('2001-06-01 21:15', tz = 'UTC'))
})
