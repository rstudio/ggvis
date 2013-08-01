library(gigvis)

df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

gigvis(df, props(x ~ x, y ~ y), mark_line())
gigvis(pipeline(df, by_group("z")), props(x ~ x, y ~ y), mark_line())