context("compute_count")

test_that("count_vector correctly handles factors", {
  fac <- factor(c("A", "A", "C"), levels = c("A", "C"))
  expect_identical(
    count_vector(fac),
    data.frame(
      count_ = c(2, 1),
      x_ = factor(c("A", "C"), levels = c("A", "C"))
    )
  )

  # Levels that aren't represented are dropped from the count
  fac <- factor(c("A", "A", "C"), levels = c("A", "B", "C"))
  expect_identical(
    count_vector(fac),
    data.frame(
      count_ = c(2, 1),
      x_ = factor(c("A", "C"), levels = c("A", "B", "C"))
    )
  )
})
