context("controls")

test_that("controls() returns named list of controls", {
  b <- branch_smooth(
    n = input_slider(min = 1, max = 10, value = 5,
      label = "Slider 1", id = "slider1"),
    method = input_select(c("LM" = "lm", "LOESS" = "loess"),
      label = "Select 1", id = "select1")
  )

  ctrls <- controls(b)

  expect_equal(ctrls$slider1, sliderInput(min = 1, max = 10, value = 5,
      label = "Slider 1", inputId = "slider1"))

  expect_equal(ctrls$select1, selectInput(choices = c("LM" = "lm", "LOESS" = "loess"),
      label = "Select 1", inputId = "select1"))
})
