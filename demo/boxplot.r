library(ggvis)

# Make data set with categorical x
mtc <- mtcars
mtc$cyl <- factor(mtc$cyl)
mtc %>% ggvis(~cyl, ~mpg) %>% layer_boxplots()
# Set the width of the boxes to half the space between tick marks
mtc %>% ggvis(~cyl, ~mpg) %>% layer_boxplots(width = 0.5)

# Continuous x: boxes fill width between data values
mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplots()
# Setting width=0.5 makes it 0.5 wide in the data space, which is 1/4 of the
# distance between data values in this particular case.
mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplots(width = 0.5)

