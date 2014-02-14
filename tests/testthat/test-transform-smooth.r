context("transform_smooth")

test_that("transform_smooth handles dates and datetimes", {

  # Data frame with POSIXct, and zigzag values
  dat <- data.frame(
    d = as.POSIXct('2001-06-11 21:00', tz = 'CDT') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- sluice(pipeline(dat, transform_smooth(n = 10, method = "lm")),
    props(x = ~d, y = ~value))
  expect_equal(range(dat$d), range(res$x))

  # Data frame with Date, and zigzag values
  dat <- data.frame(
    d = as.Date('2001-06-11') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- sluice(pipeline(dat, transform_smooth(n = 10, method = "lm")),
    props(x = ~d, y = ~value))
  expect_equal(range(dat$d), range(res$x))
})
