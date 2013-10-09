context("transform_stack")

test_that("transform_stack works as expected", {
  dat <- data.frame(
    g1 = c("a", "a", "a", "b", "b", "c", "c", "c"),
    g2 = c(1, 2, 3, 1, 3, 1, 2, 3),
    val = 1:8
  )

  # Basic stacking: stack y value at each x
  expect_equal(
    sluice(pipeline(dat, transform_stack()),
           props(x = ~g1, y = ~val)),
    cbind(dat, ymin__ = c(0,1,3,0,4,0,6,13), ymax__ = c(1,3,6,4,9,6,13,21))
  )

  # Adding another var (fill) doesn't change stacking
  expect_equal(
    sluice(pipeline(dat, transform_stack()),
           props(x = ~g1, y = ~val, fill = ~factor(g2))),
    cbind(dat, ymin__ = c(0,1,3,0,4,0,6,13), ymax__ = c(1,3,6,4,9,6,13,21))
  )

  # Stack x value at each y
  expect_equal(
    sluice(pipeline(dat, transform_stack(direction = "x")),
           props(y = ~g1, x = ~val)),
    cbind(dat, xmin__ = c(0,1,3,0,4,0,6,13), xmax__ = c(1,3,6,4,9,6,13,21))
  )

  # With grouping
  res <-sluice(pipeline(dat, by_group(g2), transform_stack()),
               props(x = ~g1, y = ~val))
  # Check that it's a proper split_df
  expect_equal(length(res), 3)
  # Convert back to data frame for easier testing of values
  res <- do.call(rbind, res)
  # Resulting data frame is same as previous, except sorted by grouping var g2
  x <- cbind(dat, ymin__ = c(0,1,3,0,4,0,6,13), ymax__ = c(1,3,6,4,9,6,13,21))
  x <- x[order(x$g2), ]
  expect_equal(res, x)

  # Expression for x and y, where x grouping doesn't match x values
  expect_equal(
    sluice(pipeline(dat, transform_stack()), props(x = ~g2 %% 2, y = ~val * 2)),
    cbind(dat, ymin__ = c(0,0,2,8,16,26,4,38), ymax__ = c(2,4,8,16,26,38,18,54))
  )
})

test_that("stacking unsorted data", {
  # Same data as previous set of tests, but randomize order
  dat <- data.frame(
    g1 = c("a", "a", "a", "b", "b", "c", "c", "c"),
    g2 = c(1, 2, 3, 1, 3, 1, 2, 3),
    val = 1:8
  )
  dat <- dat[c(7, 4, 8, 2, 6, 5, 1, 3), ]

  # Basic stacking: stack y value at each x.
  # Stacking doesn't automatically sort on other variables (like fill)
  expect_equal(
    sluice(pipeline(dat, transform_stack()),
           props(x = ~g1, y = ~val, fill = ~g2)),
    cbind(dat, ymin__ = c(0,0,7,0,15,4,2,3), ymax__ = c(7,4,15,2,21,9,3,6))
  )

  # With grouping, it automatically sorts on the grouping var (g2)
  res <-sluice(pipeline(dat, by_group(g2), transform_stack()),
               props(x = ~g1, y = ~val, fill = ~g2))
  # Convert back to data frame for easier testing of values
  res <- do.call(rbind, res)
  # Resulting data frame is same as previous, except sorted by grouping var g2
  x <- dat[order(dat$g2), ]
  x <- cbind(x, ymin__ = c(0,0,0,6,1,13,4,3), ymax__ = c(4,6,1,13,3,21,9,6))
  expect_equal(res, x)

  # No grouping, but with transform_sort in the pipeline - same order as previous
  res <- sluice(pipeline(dat, transform_sort(var = "fill"), transform_stack()),
                props(x = ~g1, y = ~val, fill = ~g2))
  expect_equal(res, x)
})
