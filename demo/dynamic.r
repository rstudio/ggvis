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
              mark_symbol(stroke = "black", fill = "black")
            ),
            node(
              data = mtc2,
              mark_symbol(fill = "red", size = 40)
            )
)
view_dynamic(p)


# With a transform
mtc1 <- reactive({invalidateLater(2000); mtcars[sample(1:nrow(mtcars), 20), ]})
p <- gigvis(data = mtc1, mapping = aes(x = "wt", y = "mpg"),
            mark_symbol(stroke = "black", fill = "black"),
            node(
              transform = transform_smooth(method = "lm", se = F),
              mark_line(stroke = "red")
            )
)
view_dynamic(p)
