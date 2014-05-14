context("compute_smooth")

test_that("compute_smooth preserves datetimes", {
  # Data frame with POSIXct, and zigzag values
  dat <- data.frame(
    d = as.POSIXct('2001-06-11 21:00', tz = 'UTC') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- dat %>% compute_smooth(value ~ d, n = 10, method = "lm")
  expect_equal(range(dat$d), range(res$pred_))
})

test_that("compute_smooth preserves dates", {
  # Data frame with Date, and zigzag values
  dat <- data.frame(
    d = as.Date('2001-06-11') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- dat %>% compute_smooth(value ~ d, n = 10, method = "lm")
  expect_equal(range(dat$d), range(res$pred_))
})

test_that("compute_smooth works with datetimes", {
  # Perfectly linear data
  dat <- data.frame(
    d = as.POSIXct('2001-06-11 21:00', tz = 'America/New_York') + 1:10 * 100,
    value = 1:10
  )

  # Tests with various models
  res <- dat %>% compute_smooth(value ~ d, n = 10, method = "loess")
  expect_equal(range(dat$d), range(res$pred_))
  expect_equal(attr(dat$d, "tzone"), attr(res$pred_, "tzone"))
  expect_equal(range(dat$value), range(res$resp_))

  res <- dat %>% compute_smooth(value ~ d, n = 10, method = "lm")
  expect_equal(range(dat$d), range(res$pred_))
  expect_equal(attr(dat$d, "tzone"), attr(res$pred_, "tzone"))
  expect_equal(range(dat$value), range(res$resp_))

  res <- dat %>% compute_smooth(value ~ d, n = 10, method = "glm")
  expect_equal(range(dat$d), range(res$pred_))
  expect_equal(attr(dat$d, "tzone"), attr(res$pred_, "tzone"))
  expect_equal(range(dat$value), range(res$resp_))
})
