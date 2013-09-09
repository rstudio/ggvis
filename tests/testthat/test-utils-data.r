context("utils-data")

test_that("finite.cases.data.frame", {
  # All finite --------------------------------------------------------------
  expect_identical(finite.cases(data.frame(x=4)),            TRUE)          # 1x1
  expect_identical(finite.cases(data.frame(x=4, y=11)),      TRUE)          # 1x2
  expect_identical(finite.cases(data.frame(x=4:5)),          c(TRUE, TRUE)) # 2x1
  expect_identical(finite.cases(data.frame(x=4:5, y=11:12)), c(TRUE, TRUE)) # 2x2

  # Has one NA --------------------------------------------------------------
  expect_identical(finite.cases(data.frame(x=NA)),                  FALSE)           # 1x1
  expect_identical(finite.cases(data.frame(x=4, y=NA)),             FALSE)           # 1x2
  expect_identical(finite.cases(data.frame(x=c(4,NA))),             c(TRUE,  FALSE)) # 2x1
  expect_identical(finite.cases(data.frame(x=c(4,NA), y=c(11,NA))), c(TRUE,  FALSE)) # 2x2
  expect_identical(finite.cases(data.frame(x=c(4,NA), y=c(NA,12))), c(FALSE, FALSE)) # 2x2
  expect_identical(finite.cases(data.frame(x=c(4,5),  y=c(NA,12))), c(FALSE, TRUE))  # 2x2

  # Testing NaN and Inf, using miscellaneous data shapes --------------------
  expect_identical(finite.cases(data.frame(x=c(4,NaN))),              c(TRUE, FALSE))
  expect_identical(finite.cases(data.frame(x=Inf)),                   FALSE)
  expect_identical(finite.cases(data.frame(x=c(4,5),  y=c(-Inf,12))), c(FALSE, TRUE))
})


test_that("remove_missing", {
  all_vals <- c(1, NA, NaN, Inf, -Inf)

  expect_warning(remove_missing(all_vals))
  expect_that(remove_missing(all_vals, warn_na = FALSE), not(gives_warning()))

  # Vectors
  expect_identical(remove_missing(all_vals, warn_na = FALSE), c(1, Inf, -Inf))
  expect_identical(remove_missing(all_vals, warn_na = FALSE, finite = TRUE), 1)

  # Data frames
  # Get all combinations
  all_combs <- data.frame(x = rep(all_vals, 5), y = rep(all_vals, each = 5))

  res <- remove_missing(all_combs, warn_na = FALSE)
  rownames(res) <- NULL
  expect_identical(
    res,
    data.frame(x = rep(c(1, Inf, -Inf), 3), y = rep(c(1, Inf, -Inf), each = 3))
  )

  expect_identical(
    remove_missing(all_combs, warn_na = FALSE, finite = TRUE),
    data.frame(x=1, y=1)
  )

})