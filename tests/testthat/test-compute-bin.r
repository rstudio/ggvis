context("compute_bin")

comp_bin <- function(...) {
  suppressMessages(compute_bin(...))
}

test_that("compute_bin preserves dates and times", {
  dates <- data.frame(val = as.Date("2013-06-01") + 0:100)
  NYtimes <- data.frame(
    val = as.POSIXct('2001-06-01 21:00', tz = 'America/New_York') + 0:10 * 100
  )
  UTCtimes <- data.frame(
    val = as.POSIXct('2001-06-01 21:00', tz = 'UTC') + seq(0, 1000, by = 10)
  )

  res <- comp_bin(dates, ~val, width = 30)
  expect_true(inherits(res$x_, "Date"))
  expect_true(inherits(res$xmin_, "Date"))
  expect_true(inherits(res$xmax_, "Date"))
  expect_identical(sum(res$count_), length(dates$val))

  res <- comp_bin(NYtimes, ~val, width = 120)
  expect_true(inherits(res$x_, "POSIXct"))
  expect_true(inherits(res$xmin_, "POSIXct"))
  expect_true(inherits(res$xmax_, "POSIXct"))
  expect_identical(sum(res$count_), length(NYtimes$val))
  expect_identical(attr(NYtimes$val, "tzone"), attr(res$x_, "tzone"))

  res <- comp_bin(UTCtimes, ~val, width = 120)
  expect_identical(sum(res$count_), length(UTCtimes$val))
  expect_identical(attr(UTCtimes$val, "tzone"), attr(res$x_, "tzone"))
})

test_that("width in lubridate::Period", {
  UTCtimes <- data.frame(
    val = as.POSIXct('2001-06-01 21:00', tz = 'UTC') + seq(0, 1000, by = 10)
  )

  # width specified as a Period from lubridate
  expect_identical(
    comp_bin(UTCtimes, ~val, width = lubridate::ms("1 42")),
    comp_bin(UTCtimes, ~val, width = 102)
  )
})

test_that("Closed left or right", {
  dat <- data.frame(x = c(0, 10))

  res <- comp_bin(dat, ~x, width = 10, pad = FALSE)
  expect_identical(res$count_, c(1L, 1L))
  res <- comp_bin(dat, ~x, width = 10, boundary = 5, pad = FALSE)
  expect_identical(res$count_, c(1L, 1L))
  res <- comp_bin(dat, ~x, width = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count_, 2L)
  res <- comp_bin(dat, ~x, width = 5, boundary = 0, pad = FALSE)
  expect_identical(res$count_, c(1L, 1L))

  res <- comp_bin(dat, ~x, width = 10, pad = FALSE, closed = "left")
  expect_identical(res$count_, c(1L, 1L))
  res <- comp_bin(dat, ~x, width = 10, boundary = 5, pad = FALSE, closed = "left")
  expect_identical(res$count_, c(1L, 1L))
  res <- comp_bin(dat, ~x, width = 10, boundary = 0, pad = FALSE, closed = "left")
  expect_identical(res$count_, c(2L))
  res <- comp_bin(dat, ~x, width = 5, boundary = 0, pad = FALSE, closed = "left")
  expect_identical(res$count_, c(1L, 1L))
})


test_that("Setting boundary and center", {
  # numeric
  dat <- data.frame(x = c(0, 30))

  # Error if both boundary and center are specified
  expect_error(comp_bin(dat, ~x, width = 10, bondary = 5, center = 0, pad = FALSE))

  res <- comp_bin(dat, ~x, width = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count, c(1L, 0L, 1L))
  expect_identical(res$xmin_[1], 0)
  expect_identical(res$xmax_[3], 30)

  res <- comp_bin(dat, ~x, width = 10, center = 0, pad = FALSE)
  expect_identical(res$count, c(1L, 0L, 0L, 1L))
  expect_identical(res$xmin_[1], dat$x[1] - 5)
  expect_identical(res$xmax_[4], dat$x[2] + 5)


  # Date
  dat <- data.frame(x = as.Date("2013-06-01") + c(0, 30))

  res <- comp_bin(dat, ~x, width = 10, boundary = as.Date("2013-06-01"), pad = FALSE)
  expect_identical(res$count_, c(1L, 0L, 1L))
  expect_identical(res$xmin_[1], dat$x[1])
  expect_identical(res$xmax_[3], dat$x[2])

  res <- comp_bin(dat, ~x, width = 10, center = as.Date("2013-06-01"), pad = FALSE)
  expect_identical(res$count, c(1L, 0L, 0L, 1L))
  expect_identical(res$xmin_[1], dat$x[1] - 5)
  expect_identical(res$xmax_[4], dat$x[2] + 5)


  # POSIXct
  dat <- data.frame(
    x = as.POSIXct('2001-06-01 21:00', tz = 'America/New_York') + c(0, 30000)
  )

  res <- comp_bin(dat, ~x, width = 10000, boundary = dat$x[1], pad = FALSE)
  expect_identical(res$count_, c(1L, 0L, 1L))
  expect_identical(res$xmin_[1], dat$x[1])
  expect_identical(res$xmax_[3], dat$x[2])

  res <- comp_bin(dat, ~x, width = 10000, center = dat$x[1], pad = FALSE)
  expect_identical(res$count, c(1L, 0L, 0L, 1L))
  expect_identical(res$xmin_[1], dat$x[1] - 5000)
  expect_identical(res$xmax_[4], dat$x[2] + 5000)
})


test_that("Automatic width", {
  dat <- data.frame(
    num = c(0, 25.0),
    num2 = c(0, 50.0),
    int = c(1L, 25L),
    int2 = c(1L, 50L),
    date = as.Date("2013-06-01") + c(0, 100),
    posixct = as.POSIXct('2001-06-01 21:00', tz = 'UTC') + c(0, 1000)
  )

  # numeric
  res <- comp_bin(dat, ~num)
  # It generates approx 30 bins, at round numbers, so should have width 1
  expect_identical(res$width_, rep(1, length(res$width_)))
  res <- comp_bin(dat, ~num2)
  expect_identical(res$width_, rep(2, length(res$width_)))

  # integer
  res <- comp_bin(dat, ~int)
  expect_true(all(res$width_ == 1L))
  res <- comp_bin(dat, ~int2)
  expect_true(all(res$width_ == 2L))

  # Date
  res <- comp_bin(dat, ~date)
  expect_identical(res$width_, rep(2, length(res$width_)))

  # POSIXct
  res <- comp_bin(dat, ~posixct)
  expect_identical(res$width_, rep(30, length(res$width_)))
})


test_that("Bin boundaries across groups", {
  # Bins should be the same across groups
  dat <- data.frame(x = c(0:2, 0:2+0.1), g=c('a','a','a', 'b','b','b'))
  res <- dat %>% group_by(g) %>% compute_bin(~x, width = 1, pad = FALSE)
  expect_identical(range(res$x_[res$g =='a']), range(res$x_[res$g =='b']))
  expect_identical(dplyr::groups(res), list(quote(g)))
  expect_identical(res$count_, rep(1L, 6))
})
