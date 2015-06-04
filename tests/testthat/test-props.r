context("props")
library(shiny)

# Recursively crawl a list and replace any environments with a special
# environment. This is a workaround for a change in behavior in R 3.2.0
# for all.equal when given two environments.
blank_envs <- function(x) {
  replacement_env <- new.env(parent = emptyenv())

  # Use `x[]<-` to preserve any attributes on x
  x[] <- lapply(x, function(val) {
    if (is.environment(val)) replacement_env
    else if (is.list(val)) blank_envs(val)
    else val
  })

  x
}

test_prop <- function(p, property, value, scale, event = "update") {
  expect_identical(p$property, property)
  expect_identical(p$value, value)
  expect_identical(p$scale, scale)
  expect_identical(p$event, event)
}

test_that("creating prop objects with prop()", {
  expect_error(prop("y")) # Need value
  expect_error(prop(x = 1)) # Need scale

  # Unscaled, constant
  test_prop(prop("x", 1, scale = FALSE), "x", 1, NULL)
  test_prop(prop("x", 1), "x", 1, NULL)   # Use default scale
  test_prop(prop("x", ~1), "x", 1, "x")
  test_prop(prop("x2", 1), "x2", 1, NULL)

  # Unscaled, variable, with quote() and ~
  test_prop(prop("x", quote(cyl), scale = FALSE), "x", quote(cyl), NULL)
  test_prop(prop("x2", ~cyl, scale = FALSE), "x2", quote(cyl), NULL)

  # Scaled, constant
  test_prop(prop("x", 1, scale = "x"), "x", 1, "x")
  test_prop(prop("x2", 1, scale = "x"), "x2", 1, "x")
  test_prop(prop("fill", 1, scale = "foo"), "fill", 1, "foo")

  # Scaled, variable
  test_prop(prop("x", quote(cyl)), "x", quote(cyl), "x")   # Use default scale
  test_prop(prop("x2", ~cyl), "x2", quote(cyl), "x")
  test_prop(prop("x", quote(cyl), scale = "x"), "x", quote(cyl), "x")
  test_prop(prop("x", ~cyl, scale = "foo"), "x", quote(cyl), "foo")

  # Event is used
  test_prop(prop("x", 1, event = "update"), "x", 1, NULL, "update")
  test_prop(prop("x", 1, event = "enter"), "x", 1, NULL, "enter")

  # key doesn't get an event
  test_prop(prop("key", ~id, scale = FALSE), "key", quote(id), NULL, NULL)
  expect_error(prop("key", ~id, event = "update"))
  # scale must be FALSE
  expect_error(prop("key", ~id))
  expect_error(prop("key", ~id, scale = TRUE))
  # can't be constant
  expect_error(prop("key", 1:10))
})

test_that("property names for variables", {
  # pname(wt) is equivalent to prop_label(prop(quote(wt)))
  pname <- function(x) prop_label(prop("x", substitute(x)))

  # Regular variable names are simply converted to string
  expect_identical(pname(wt), "wt")

  # Variable names with special characters are made vega-safe
  expect_identical(pname(`wt/mpg`), "wt/mpg")
  expect_identical(pname(`wt.mpg`), "wt.mpg")

  # safe_vega_var escapes "."
  expect_identical(safe_vega_var("wt.mpg"), "wt\\.mpg")

  # Expressions are kept as is
  expect_identical(pname(wt/mpg), "wt/mpg")

  # Expressions and weird column names will produce same name
  # (but this is very unlikely)
  expect_true(pname(wt/mpg) == pname(`wt/mpg`))

  # Constants have no variable name
  expect_identical(pname(10), "")
  expect_identical(pname("wt"), "")
})

test_that("property values for variables", {
  # Create a data frame with columns 'a', 'b', and 'b-a'
  dat <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  names(dat)[3] <- "b-a"

  # Column named `b-a`
  expect_identical(prop_value(prop("x", quote(`b-a`)), dat), 7:9)

  # Expression b-a
  expect_equal(prop_value(prop("x", quote(b-a)), dat), c(3, 3, 3))

  # Constant prop
  expect_equal(prop_value(prop("x", 10), dat), rep(10, 3))

  # Expression which returns a single value; value should be replicated
  expect_equal(prop_value(prop("x", quote(min(a))), dat), c(1, 1, 1))

  # Expression which returns # of values that doesn't divide into # of rows
  expect_error(prop_value(prop("x", quote(range(a))), dat))
})

test_that("prop captures environment for evaluation", {
  dat <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  b <- 11:12
  d <- 13:14

  # Column from data
  expect_identical(prop_value(prop("x", quote(a)), dat), 1:2)

  # Column from data, even though a var in the calling env has same name
  expect_identical(prop_value(prop("x", quote(b)), dat), 3:4)

  # Variable in calling environment
  expect_identical(prop_value(prop("x", quote(d)), dat), 13:14)

  # Column from data with variable in calling environment
  expect_identical(prop_value(prop("x", quote(b+d)), dat), 3:4 + 13:14)

  # Column from data with variable in calling environment
  expect_identical(prop_value(prop("x", quote(b+d)), dat), 3:4 + 13:14)


  # Create new environment for storing variables
  env <- new.env()
  env$d <- 23:24

  # Variable in env
  expect_identical(prop_value(prop("x", quote(d), env = env), dat), 23:24)
  # Column from data, even though a var in env has same name
  expect_identical(prop_value(prop("x", quote(b), env = env), dat), 3:4)
})

test_that("props() creates correct prop objects", {
  # 6 combinations of := and constant/variable/reactive
  p <- props(x := 1, y := ~mpg, x2 := reactive(cyl),
             stroke = "foo", fill = ~wt, size = reactive(am))
  expect_equal(sort(names(p)),
    sort(c("x.update", "y.update", "x2.update",
           "stroke.update", "fill.update", "size.update"))
  )

  test_prop(p$x.update, "x", 1, NULL)
  test_prop(p$y.update, "y", quote(mpg), NULL)
  # Reactives are hard to test with identical, so test the components
  expect_identical(p$x2.update$property, "x2")
  expect_true(is.reactive(p$x2.update$value))
  expect_identical(p$x2.update$scale, NULL)

  test_prop(p$stroke.update, "stroke", "foo", "stroke")
  test_prop(p$fill.update, "fill", quote(wt), "fill")
  expect_identical(p$size.update$property, "size")
  expect_true(is.reactive(p$size.update$value))
  expect_identical(p$size.update$scale, "size")


  # Properties get default scales (e.g. x2 gets scale x)
  p <- props(x2 = ~cyl)
  test_prop(p$x2.update, "x2", quote(cyl), "x")

  # Explicitly-created prop objects
  p <- props(prop("fill", "foo"), prop("x2", ~cyl))
  test_prop(p$fill.update, "fill", "foo", NULL)
  test_prop(p$x2.update, "x2", quote(cyl), "x")

  # Unnamed arguments
  expect_error(props(~wt, ~mpg, 1))  # Too many
  p <- props(~wt, 1)
  test_prop(p$x.update, "x", quote(wt), "x")
  test_prop(p$y.update, "y", 1, "y")

  # Unnamed arguments, out of order
  p <- props(~mpg, prop("fill", ~cyl), x = ~wt)
  test_prop(p$x.update, "x", quote(wt), "x")
  test_prop(p$y.update, "y", quote(mpg), "y")
  test_prop(p$fill.update, "fill", quote(cyl), "fill")

  # Prop objects, stored in variables
  p1 <- prop("fill", ~cyl)
  p2 <- 1
  p <- props(p1, p2)
  test_prop(p$x.update, "x", 1, "x")
  test_prop(p$fill.update, "fill", quote(cyl), "fill")

  # Prop objects with event (.enter, .exit, .update, .hover)
  p <- props(x.exit := 1, prop("fill", ~cyl, event = "enter"))
  test_prop(p$x.exit, "x", 1, NULL, "exit")
  test_prop(p$fill.enter, "fill", quote(cyl), "fill", "enter")

  # x.enter doesn't block automatic naming for x.update
  p <- props(x.enter := 1, ~mpg)
  test_prop(p$x.enter, "x", 1, NULL, "enter")
  test_prop(p$x.update, "x", quote(mpg), "x", "update")

  # key is special
  p <- props(key := ~id)
  test_prop(p$key, "key", quote(id), NULL, NULL)
  expect_error(props(key = ~id)) # Must be unscaled
  expect_error(props(key := 1:10)) # Can't be constant
})

test_that("props evaluates arguments in correct environment", {
  p <- 1
  f <- function() {
    p <- 2
    props(p)
  }
  expect_identical(f()$x.update$value, 2)
})

test_that("props uses environment in formulas", {
  dat <- data.frame(a = 1:2, b = 3:4)
  val <- 11:12

  # Create a formula that captures the function environment
  gen_formula <- function() {
    val <- 21:22
    ~ val
  }

  p <- props(w = ~val, x := gen_formula(), y := 5, z := 6)

  # Should get val from this environment, where w~val was defined
  expect_identical(prop_value(p$w, dat), 11:12)

  # Should get val from gen_formula environment, where x~val was defined
  expect_identical(prop_value(p$x, dat), 21:22)
})

test_that("merging props", {
  # Utility function: sort props by name
  sortp <- function(p) p[sort(names(p))]

  p_i  <- blank_envs(props(x=~a, z:="red"))
  q_i  <- blank_envs(props(y=~b, z:="blue"))
  q_ni <- blank_envs(props(y=~b, z:="blue", inherit=FALSE))

  expect_equal(sortp(merge_props(p_i, q_i)), blank_envs(props(x=~a, y=~b, z:="blue")))
  expect_equal(sortp(merge_props(p_i, q_ni)), q_ni)
  expect_equal(
    sortp(merge_props(q_ni, p_i)),
    blank_envs(props(x=~a, y=~b, z:="red", inherit = FALSE))
  )
})

test_that("prop_event_sets splits up props properly", {
  p <- props(x.update = ~wt, x.enter = 1, y = ~mpg, fill.hover := "red")
  ps <- prop_event_sets(p)

  expect_equal(names(ps), c("enter", "update", "hover"))
  expect_equal(names(ps$enter), "x")
  expect_equal(names(ps$update), c("x", "y"))
  expect_equal(names(ps$hover), "fill")

  expect_identical(ps$enter$x, prop("x", 1, scale = TRUE, event = "enter"))
  expect_identical(ps$update$x, prop("x", quote(wt), event = "update"))
  expect_identical(ps$update$y, prop("y", quote(mpg), event = "update"))
  expect_identical(ps$hover$fill, prop("fill", "red", event = "hover"))

  expect_true(attr(ps$enter, "inherit"))
  expect_true(attr(ps$update, "inherit"))
  expect_true(attr(ps$hover, "inherit"))

  # value of inherit is passed along
  p <- props(x = ~wt, x.enter = 1, inherit = FALSE)
  ps <- prop_event_sets(p)
  expect_false(attr(ps$enter, "inherit"))
  expect_false(attr(ps$update, "inherit"))
})

test_that("drop_props", {
  p <- props(x = ~wt, x.enter = 0, stroke.enter := "black", stroke.hover := "red")

  expect_identical(
    drop_props(p, c("stroke", "strokeOpacity")),
    props(x = ~wt, x.enter = 0)
  )

  # Use unname() because it drop_props returns an named empty list as opposed
  # to an unnamed empty list.
  expect_identical(
    unname(drop_props(p, c("x", "stroke"))),
    props()
  )
})

test_that("band() is created properly", {
  # Automatic setting of scale, event
  test_prop(prop("width", band()), "width", NULL, "x", "update")
  test_prop(prop("height", band()), "height", NULL, "y", "update")

  # Explicit settings of scale, event
  test_prop(prop("width", band(), scale = "x"), "width", NULL, "x")
  test_prop(prop("width", band(), event = "enter"), "width", NULL, "x", "enter")
  test_prop(prop("width", band(), scale = "foo"), "width", NULL, "foo")

  # Create with props()
  test_prop(props(width = band())$width.update, "width", NULL, "x", "update")
  test_prop(props(height.enter = band())$height.enter, "height", NULL, "y", "enter")

  # Error if property isn't width or height
  expect_error(prop("x", band()))
  expect_error(props(x = band()))
})
