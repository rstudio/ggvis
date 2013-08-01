delayed_reactive <- function(fun, controls = NULL) { 
  if (is.character(fun)) {
    fun <- from_input(fun)
  }
  stopifnot(is.function(fun))

  structure(list(fun = fun, controls = controls), class = "delayed_reactive")
}


from_input <- function(id) {
  call <- substitute(function(session) session$input[[id]], list(id = id))
  eval(call)
}

#' @S3method print delayed_reactive
print.delayed_reactive <- function(x, ...) {
  cat("<delayed_reactive>\n")
  print(body(x$fun))
}

#' @S3method as.reactive delayed_reactive
as.reactive.delayed_reactive <- function(x, session = NULL, ...) {
  if ("session" %in% names(formals(x$fun))) {
    reactive(x$fun(session = session))
  } else {
    reactive(x$fun())
  }
}
