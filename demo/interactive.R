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
    method = input_radiobuttons(choices = c("LOESS" = "loess", "Linear" = "lm"),
                                label = "Model type"),
    props(stroke = prop(input_radiobuttons(
        choices = c("Red" = "red", "Black" = "black"), label = "Line color"))
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
        # Build a RGB color string
        str = "#"
        str <- paste0(str, if ("r" %in% val) "f0" else "00")
        str <- paste0(str, if ("g" %in% val) "f0" else "00")
        str <- paste0(str, if ("b" %in% val) "f0" else "00")
        str
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
    fill = prop(input_select(c("red", "blue"), label = "Color")),
    size = prop(input_slider(10, 1000, 100, label = "Size")),
    opacity = prop(input_slider(0, 1, 1, label = "Opacity"))
  ),
  mark_symbol()
)


# Constant values, on a scale
ggvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = prop(
      input_select(c("Set A" = "A", "Set B" = "B"),
        label = "Dynamically-generated column",
        map = function(value) {
          switch(value,
            "A" = c("One", "Two"),
            "B" = c("First", "Second", "Third", "Fourth"))
        }),
      scale = TRUE)
  ),
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
    fill = prop(input_select(c("colour1", "colour2"), map = as.name), scale = FALSE)
  ),
  mark_symbol()
)

# Variable values
ggvis(mtcars,
  props(
    x ~ wt,
    y ~ mpg,
    fill = prop(input_select(c("mpg", "wt"), map = as.name), scale = TRUE)
  ),
  mark_symbol()
)
