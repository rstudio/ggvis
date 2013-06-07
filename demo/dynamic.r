library(gigvis)
library(shiny)

# Two separate data sets, equal in the tree
mtc1 <- reactive({invalidateLater(2000); mtcars[sample(1:10, 5), ]})
mtc2 <- reactive({invalidateLater(2000); mtcars[sample(11:20, 5), ]})
# mtc1 <- mtcars[1:10, ]
# mtc2 <- mtcars[11:20, ]
p <- gigvis(data = NULL, mapping = aes(x = "wt", y = "mpg"),
            node(
              data = mtc1,
              mark_point(stroke = "black", fill = "black")
            ),
            node(
              data = mtc2,
              mark_point(fill = "red", size = 40)
            )
)
view_dynamic(p)
