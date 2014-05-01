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
