context("functional")

test_that("defer() inserts argument in the right place", {
  expect_equal(defer(a()), function(.) a(.))
  expect_error(defer(a))
  expect_equal(defer(a(10)), function(.) a(., 10))
  expect_equal(defer(a() %>% b()), function(.) a(.) %>% b())
  expect_error(defer(a %>% b))
  expect_equal(defer(b(a())), function(.) b(., a()))
  expect_equal(defer(a() %>% b() %>% c()), function(.) a(.) %>% b() %>% c())
  expect_equal(defer(a(10) %>% b() %>% c()), function(.) a(., 10) %>% b() %>% c())

  # Other operators
  expect_equal(defer(a(1) + 2), function(.) a(., 1) + 2)
  expect_equal(defer(-a(1)), function(.) -a(., 1))

  # Anonymous functions
  expect_equal(defer((function(x) x+5)()), function(.) (function(x) x + 5)(.))
  expect_equal(defer((function(x, y) x+y)(y)), function(.) (function(x, y) x+y)(., y))
})
