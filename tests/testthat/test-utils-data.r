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


test_that("concat preserves types and timezones", {
  expect_identical(concat(list(1:3, 3:5)), c(1:3, 3:5))
  expect_identical(
    concat(list(c('a', 'b'), c('a', 'c'))),
    c('a', 'b', 'a', 'c')
  )
  expect_identical(
    concat(list(factor(c('a', 'b')), factor(c('c', 'a')))),
    factor(c('a', 'b', 'c', 'a'))
  )
  # Factors with different level order
  expect_identical(
    concat(list(factor(c('a', 'b'), levels = c('a', 'c', 'b')),
                factor(c('c', 'a')))),
    factor(c('a', 'b', 'c', 'a'), levels = c('a', 'c', 'b'))
  )
  expect_identical(
    concat(list(factor(c('a', 'b'), levels = c('b', 'a')),
                factor(c('c', 'd'), levels = c('d', 'c')))),
    factor(c('a', 'b', 'c', 'd'), levels = c('b', 'a', 'd', 'c'))
  )


  # Preserves time zone
  t1 <- as.POSIXct('2001-06-11 21:00', tz = 'UTC') + c(0, 2000)
  t2 <- t1 + 5000
  expect_identical(
    concat(list(t1, t2)),
    as.POSIXct('2001-06-11 21:00', tz = 'UTC') + c(0, 2000, 5000, 7000)
  )


  # Lists with 3 items
  expect_identical(concat(list(1:3, 3:5, 1:2)), c(1:3, 3:5, 1:2))
  expect_identical(
    concat(list(factor(c('a', 'b')), factor(c('a', 'c')), factor(c('b', 'd')))),
    factor(c('a', 'b', 'a', 'c', 'b', 'd'))
  )
  expect_identical(
    concat(list(t1, t2, t2)),
    as.POSIXct('2001-06-11 21:00', tz = 'UTC') + c(0, 2000, 5000, 7000, 5000, 7000)
  )
})


test_that("concat handles NULLs and zero-length vectors", {
  expect_identical(concat(list(NULL, NULL)), NULL)
  expect_identical(concat(list(NULL, character(0))), character(0))
  expect_identical(concat(list(NULL, 1:10)), 1:10)
  expect_identical(concat(list(1:10, NULL, integer(0))), 1:10)
})


test_that("preserve_constants", {
  # Input data frames with various numbers of rows
  input0 <- data.frame(a = numeric(0), b = character(0), stringsAsFactors = FALSE)
  input1 <- data.frame(a = 1, b = "txt", stringsAsFactors = FALSE)
  input3 <- data.frame(a = 1:3, b = rep("txt", 3), stringsAsFactors = FALSE)

  # Output data frames with various numbers of rows
  output0 <- data.frame(foo = numeric(0))
  output1 <- data.frame(foo = 11)
  output2 <- data.frame(foo = 11:12)

  # Some things that the input data frames get reduced to
  abNA <- data.frame(a = NA_real_, b = NA_character_, stringsAsFactors = FALSE)
  b0 <- data.frame(b = character(0), stringsAsFactors = FALSE)
  b1 <- data.frame(b = "txt", stringsAsFactors = FALSE)

  # Test all the combinations
  expect_identical(preserve_constants(input0, output0), cbind(input0, output0))
  expect_identical(preserve_constants(input1, output0), cbind(input0, output0))
  expect_identical(preserve_constants(input3, output0), cbind(b0, output0))

  expect_identical(preserve_constants(input0, output1), cbind(abNA, output1))
  expect_identical(preserve_constants(input1, output1), cbind(input1, output1))
  expect_identical(preserve_constants(input3, output1), cbind(b1, output1))

  expect_identical(preserve_constants(input0, output2), cbind(abNA, output2))
  expect_identical(preserve_constants(input1, output2), cbind(input1, output2))
  expect_identical(preserve_constants(input3, output2), cbind(b1, output2))

  # grouped_df with no rows in some groups - output shouldn't have NA rows for
  # those that are missing in one or the other
  input3g <- group_by(input3, a)
  expect_equal(
    preserve_constants(input3g, data.frame(a=1:2, v=5:6)),
    group_by(data.frame(a=1:2, b=c("txt","txt"), v=5:6, stringsAsFactors=FALSE), a)
  )
})

test_that("preserve_constants preserves factor level order", {
  input <- data.frame(g1 = factor(c('A','B','C'), levels = c('B','A','C')))
  output <- data.frame(
    g1 = factor(c('A','B','C'), levels = c('B','A','C')),
    g2 = factor(c('A','B','C'), levels = c('B','A','C'))
  )

  expect_identical(preserve_constants(input, output), output)

  # grouped
  res <- preserve_constants(group_by(input, g1), group_by(output, g1))
  expect_identical(levels(res$g1), c('B','A','C'))
  expect_identical(levels(res$g2), c('B','A','C'))
})

test_that("to_csv", {
  # Zero-row data frame. The trailing \n should be optional.
  expect_identical(
    to_csv(data.frame(x = numeric(0), c = character(0))),
    "\"x\",\"c\"\n"
  )
})
