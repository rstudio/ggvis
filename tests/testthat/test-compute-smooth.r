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

test_that("compute_smooth works with more complex formulas", {
  dat <- data.frame(x = 1:10, y = (1:10 - 5)^2 + 4 * 1:10 + 100)
  res <- dat %>% compute_smooth(y ~ I(x^2) + x, n = 10, method = "lm") %>%
          setNames( c("x", "y"))
  expect_equal(dat, res)

  dat <- data.frame(x = 1:10, y = 2.5*(1:10)^3 + 7*(1:10)^2 + 4*(1:10) + 100)
  res <- dat %>% compute_smooth(y ~ poly(x, 3), n = 10, method = "lm") %>%
          setNames( c("x", "y"))
  expect_equal(dat, res)
})
