library(gigvis)
library(shiny)
options(shiny.reactlog = TRUE)

df <- data.frame(x = 1:10, y = 1:10)
asis <- props(x ~ x, y ~ y)

v <- reactiveValues(add = 0, mult = 1, n = 5)
p <- pipeline(
  reactive(df[1:v$n, , ]), 
  transform_scale(add = reactive(v$add), mult = reactive(v$mult))
)
r <- connect(p, asis)
isolate(r())
v$add <- 2
v$n <- 10
isolate(r())

showReactLog()