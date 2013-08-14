library(gigvis)

# Slider and select input in a transform
gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    n = input_slider(2, 80, value = 5, step = 1, label = "Interpolation points"),
    method = input_select(c("Linear" = "lm", "LOESS" = "loess"), label = "Method")
  )
)

gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    method = "loess", formula = mpg ~ wt,
    span = input_slider(0.2, 1, label = "span"))
)

# Example with value wrapper function
gigvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    n = input_select(
      choices = c("Two", "Six", "Eighty"),
      wrapfun = function(value) {
        switch(value, Two = 2, Six = 6, Eighty = 80)
      },
      label = "Number of points"
    )
  )
)

# Reactive properties -------------------------------------------------------
# Constant values, raw (not on a scale)
gigvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = prop_reactive(input_select(c("red", "blue"), label = "Color"),
                         constant = TRUE, scale = FALSE),
    size = prop_reactive(input_slider(10, 1000, 100, label = "Size"),
                         constant = TRUE, scale = FALSE),
    opacity = prop_reactive(input_slider(0, 1, 1, label = "Opacity"),
                            constant = TRUE, scale = FALSE)

  ),
  mark_symbol()
)


# Constant values, on a scale
gigvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = prop_reactive(input_select(c("a", "b")),
                         constant = TRUE, scale = TRUE)
  ),
  mark_symbol()
)

# Variable values, raw (not on a scale)
mtc <- mtcars
mtc$colour1 <- c("red", "black")
mtc$colour2 <- c("blue", "gray")
gigvis(mtc,
  props(
    x ~ wt,
    y ~ mpg,
    fill = prop_reactive(input_select(c("colour1", "colour2")),
                         constant = FALSE, scale = FALSE)
  ),
  mark_symbol()
)

# Variable values
gigvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = prop_reactive(input_select(c("mpg", "wt")),
                         constant = FALSE, scale = TRUE)
  ),
  mark_symbol()
)
