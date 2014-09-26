context("compute_model_prediction")

test_that("compute_model_prediction preserves datetimes", {
  # Data frame with POSIXct, and zigzag values
  dat <- data.frame(
    d = as.POSIXct('2001-06-11 21:00', tz = 'UTC') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- dat %>% compute_model_prediction(value ~ d, n = 10, model = "lm")
  expect_equal(range(dat$d), range(res$pred_))
})

test_that("compute_model_prediction preserves dates", {
  # Data frame with Date, and zigzag values
  dat <- data.frame(
    d = as.Date('2001-06-11') + seq(1, 1000, by = 10),
    value = 1:100 + rep(c(-3, 3), 50)
  )
  res <- dat %>% compute_model_prediction(value ~ d, n = 10, model = "lm")
  expect_equal(range(dat$d), range(res$pred_))
})

test_that("compute_model_prediction works with datetimes", {
  # Perfectly linear data
  dat <- data.frame(
    d = as.POSIXct('2001-06-11 21:00', tz = 'America/New_York') + 1:10 * 100,
    value = 1:10
  )

  # Tests with various models
  res <- dat %>% compute_model_prediction(value ~ d, n = 10, model = "loess")
  expect_equal(range(dat$d), range(res$pred_))
  expect_equal(attr(dat$d, "tzone"), attr(res$pred_, "tzone"))
  expect_equal(range(dat$value), range(res$resp_))

  res <- dat %>% compute_model_prediction(value ~ d, n = 10, model = "lm")
  expect_equal(range(dat$d), range(res$pred_))
  expect_equal(attr(dat$d, "tzone"), attr(res$pred_, "tzone"))
  expect_equal(range(dat$value), range(res$resp_))

  res <- dat %>% compute_model_prediction(value ~ d, n = 10, model = "glm")
  expect_equal(range(dat$d), range(res$pred_))
  expect_equal(attr(dat$d, "tzone"), attr(res$pred_, "tzone"))
  expect_equal(range(dat$value), range(res$resp_))
})

test_that("compute_model_prediction works with more complex formulas", {
  dat <- data.frame(x = 1:10, y = (1:10 - 5)^2 + 4 * 1:10 + 100)
  res <- dat %>%
          compute_model_prediction(y ~ I(x^2) + x, n = 10, model = "lm") %>%
          setNames( c("x", "y"))
  expect_equal(dat, res)

  dat <- data.frame(x = 1:10, y = 2.5*(1:10)^3 + 7*(1:10)^2 + 4*(1:10) + 100)
  res <- dat %>%
          compute_model_prediction(y ~ poly(x, 3), n = 10, model = "lm") %>%
          setNames( c("x", "y"))
  expect_equal(dat, res)
})


test_that("Can control domain", {
  dat <- data.frame(x = 1:10, y = 5 * (1:10))
  res <- dat %>%
          compute_model_prediction(y ~ x, n = 10, model = "lm", domain = c(11, 20)) %>%
          setNames( c("x", "y"))
  expect_equal(res, data.frame(x = 11:20, y = 5 * (11:20)))
})


test_that("Zero-row inputs", {
  res <- mtcars[0,] %>% compute_model_prediction(wt ~ mpg, model = "lm")
  expect_equal(nrow(res), 0)
  expect_true(setequal(names(res), c("pred_", "resp_")))

  res <- mtcars[0,] %>% compute_model_prediction(wt ~ mpg, model = "lm", se = TRUE)
  expect_equal(nrow(res), 0)
  expect_true(setequal(
    names(res),
    c("pred_", "resp_", "pred_lwr_", "pred_upr_", "pred_se_" )
  ))

  # Smooth
  res <- mtcars[0,] %>% compute_smooth(wt ~ mpg)
  expect_equal(nrow(res), 0)
  expect_true(setequal(names(res), c("pred_", "resp_")))

  # Grouped
  res <- mtcars[0,] %>% group_by(cyl) %>%
    compute_model_prediction(wt ~ mpg, model = "lm", se = FALSE)
  expect_true(setequal(names(res), c("cyl", "pred_", "resp_")))

  res <- mtcars[0,] %>% group_by(cyl) %>%
    compute_model_prediction(wt ~ mpg, model = "lm", se = TRUE)
  expect_true(setequal(
    names(res),
    c("cyl", "pred_", "resp_", "pred_lwr_", "pred_upr_", "pred_se_" )
  ))
})
