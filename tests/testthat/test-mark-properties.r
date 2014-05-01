context("Mark: properties")

# FIXME!!
# test_that("check_mark_props returns true if all ok", {
#   expect_true(check_mark_props(mark_point(), c("x", "y", "stroke")))
#   expect_true(check_mark_props(mark_rect(), c("x", "x2", "y", "y2")))
# })
#
# test_that("check_mark_props returns helpful suggestion for single incorrect", {
#   expect_error(check_mark_props(mark_point(), "Stroke"), "stroke")
#   expect_error(check_mark_props(mark_point(), "strke"), "stroke")
# })
#
# test_that("check_mark_props doesn't give suggestion if really wrong", {
#   expect_that(check_mark_props(mark_point(), "asdfasdfdsa"),
#               not(throws_error("Did you mean")))
# })
#
#
# test_that("check_mark_props returns helpful suggestion for mixture", {
#   expect_error(check_mark_props(mark_point(), c("x", "Stroke")), "stroke")
#   expect_error(check_mark_props(mark_point(), c("x", "strke")), "stroke")
# })
