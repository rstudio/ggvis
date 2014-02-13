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

  # Data frame with POSIXlt, and zigzag values
  dat <- data.frame(
    value = 1:100 + rep(c(-3, 3), 50)
  )
  # Need to add d separately, because data.frame() would coerce it to POSIXct
  dat$d <- as.POSIXlt('2001-06-11 21:00', tz = 'America/New_York') + seq(1, 1000, by = 10)
  res <- sluice(pipeline(dat, transform_smooth(n = 10, method = "lm")),
    props(x = ~d, y = ~value))
  expect_equal(range(dat$d), range(res$x))

  # Note that there are weird things about time zones with POSIXlt:
  # t1 <- as.POSIXlt("2010-07-01 16:00:00", tz="America/New_York")
  # attr(t1, 'tzone')
  # # [1] "America/New_York"
  #
  # t2 <- as.POSIXct("2010-07-01 16:00:00", tz="America/New_York")
  # t2 <- as.POSIXlt(t2)
  # attr(t2, 'tzone')
  # # [1] "America/New_York" "EST"              "EDT"


  # Data frame with Date, and zigzag values
  dat <- data.frame(
    d = as.Date('2001-06-11') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- sluice(pipeline(dat, transform_smooth(n = 10, method = "lm")),
    props(x = ~d, y = ~value))
  expect_equal(range(dat$d), range(res$x))
})
