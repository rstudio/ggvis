delayed_reactive <- function(fun, controls = NULL) { 
  stopifnot(is.function(fun))
  stopifnot(names(formals(fun)) == "session")

  structure(list(fun = fun, controls = controls), class = "delayed_reactive")
}

print.delayed_reactive <- function(x, ...) {
  cat("<delayed_reactive>\n")
  cat(format(body(x$fun)))
}
