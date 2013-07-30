library(gigvis)

df <- data.frame(x = runif(30), y = runif(30), z = gl(3, 10))

gigvis("df", props(x ~ x, y ~ y), 
  mark_line())

gigvis(pipeline("df", by_group("z")), props(x ~ x, y ~ y),
  mark_line())