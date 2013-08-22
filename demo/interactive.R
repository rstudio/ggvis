library(ggvis)

# Slider and select input in a transform
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    n = input_slider(2, 80, value = 5, step = 1, label = "Interpolation points"),
    method = input_select(c("Linear" = "lm", "LOESS" = "loess"), label = "Method")
  )
)

ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    method = "loess", formula = mpg ~ wt,
    span = input_slider(0.2, 1, label = "span"))
)

# Example with value map function
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    n = input_select(
      choices = c("Two", "Six", "Eighty"),
      map = function(value) switch(value, Two = 2, Six = 6, Eighty = 80),
      label = "Number of points"
    )
  )
)

# Checkbox input
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(opacity = input_checkbox(label = "Semi-transparent",
                                   map = function(val) ifelse(val, .3, 1)))
  ),
  branch_smooth(
    method = input_checkbox(label = "LOESS (curve) model fit",
                            map = function(val) ifelse(val, "loess", "lm")))
)

# Text input
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(fill = input_text(label = "Point color", value = "red"))
  ),
  branch_smooth(
    method = input_text(label = "Model type", value = "loess")
  )
)

# Numeric input
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(size = input_numeric(value = 25, label = "Point size"))
  ),
  branch_smooth(
    n = input_numeric(value = 5, label = "Interpolation points")
  )
)

# Radio buttons
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(),
  branch_smooth(
    method = input_radiobuttons(c("LOESS" = "loess", "Linear" = "lm"),
      label = "Model type"),
    props(stroke = input_radiobuttons(c("Red" = "red", "Black" = "black"),
      label = "Line color")
    )
  )
)

# Checkbox group
ggvis(mtcars, props(x ~ wt, y ~ mpg),
  mark_symbol(
    props(fill = input_checkboxgroup(
      choices = c("Red" = "r", "Green" = "g", "Blue" = "b"),
      label = "Point color components",
      map = function(val) {
        rgb(0.8 * "r" %in% val, 0.8 * "g" %in% val, 0.8 * "b" %in% val)
      }
    ))
  )
)

# Reactive properties -------------------------------------------------------
# Constant values, raw (not on a scale)
ggvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = input_select(c("red", "blue"), label = "Color"),
    size = input_slider(10, 1000, 100, label = "Size"),
    opacity = input_slider(0, 1, 1, label = "Opacity")
  ),
  mark_symbol()
)


# Constant values, on a scale
new_vals <- input_select(c("Set A" = "A", "Set B" = "B"),
  label = "Dynamically-generated column",
  map = function(value) {
    vals <- switch(value,
      "A" = rep(c("One", "Two")),
      "B" = c("First", "Second", "Third", "Fourth"))
    rep(vals, length = nrow(mtcars))
  }, scale = TRUE)

ggvis(mtcars,
  props(x ~ wt, y ~ mpg, fill = new_vals),
  mark_symbol()
)

# Variable values, raw (not on a scale)
mtc <- mtcars
mtc$colour1 <- c("red", "black")
mtc$colour2 <- c("blue", "gray")
ggvis(mtc,
  props(
    x ~ wt,
    y ~ mpg,
    fill = input_select(c("colour1", "colour2"), map = as.name)
  ),
  mark_symbol()
)

# Variable values
ggvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = input_select(c("mpg", "wt"), map = as.name, scale = TRUE)
  ),
  mark_symbol()
)
