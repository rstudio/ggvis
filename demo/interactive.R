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

# Reactive properties
gigvis(mtcars, props(x ~ wt, y ~ mpg,
  fill = prop_reactive(input_select(c("red", "blue")),
                      constant = TRUE, scale = FALSE)),
  mark_symbol()
)
