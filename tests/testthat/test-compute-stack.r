context("compute_stack")

test_that("compute_stack works as expected", {
  dat <- data.frame(
    g1 = c("a", "a", "a", "b", "b", "c", "c", "c"),
    g2 = c(1, 2, 3, 1, 3, 1, 2, 3),
    val = 1:8
  )

  # Basic stacking: stack y value at each x
  stacked <- dat %>% compute_stack(~val, ~g1) %>% as.data.frame()
  expected <- data.frame(
    stack_lwr_ = c(0,1,3,0,4,0,6,13),
    stack_upr_ = c(1,3,6,4,9,6,13,21)
  )

  expect_equal(stacked[c("stack_lwr_", "stack_upr_")], expected)

  # With grouping
  stacked <- dat %>% group_by(g2) %>% compute_stack(~val, ~g1) %>%
    as.data.frame()
  expected <- expected[order(dat$g2), ] %>% as.data.frame()
  expected <- expected[order(dat$g2), ] %>% as.data.frame()
  expect_equal(stacked[c("stack_lwr_", "stack_upr_")], expected)
})


test_that("compute_stack works on zero-row data", {
  dat <- data.frame(a = numeric(0), b = character(0))
  stacked <- compute_stack(dat, ~a, ~b)
  expect_equal(nrow(stacked), 0)
  expect_identical(
    names(stacked),
    c("a", "b", "group__", "stack_upr_", "stack_lwr_")
  )
})
