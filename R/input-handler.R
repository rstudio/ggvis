#' Create handler S3 class.
#' 
#' This is currently a subclass on input, but it should probably be the
#' other way around since inputs are handlers that have controls.
#' 
#' @export
#' @param subclass name of the subclass. \code{handler} is an abstract base 
#'   class so this must always be provided.
#' @param listener name of the js listener (with corresponding R event broker)
#'   that this handler is associated with
#' @inheritParams input
#' @examples
#' p <- ggvis(mtcars, props(x = ~mpg, y = ~wt, size = left_right(1, 100)),
#'   mark_symbol())
#' p$props$size.update$dr
#' 
#' # Handlers are extracted with the internal handlers() function
#' # ggvis:::handlers(p)
handler <- function(subclass, listener, control_args = list(), value = NULL, 
                    map = identity, id = rand_id()) {
  assert_that(is.string(listener))
  
  out <- input("", control_args = control_args, value = value, 
    map = map, id = id)
  class(out) <- c(subclass, "handler", "input")
  
  # Hack around current bad class design
  out$listener <- listener
  out$control_f <- NULL
  out
}

#' @export
as.vega.handler <- function(x, session = NULL, dynamic = FALSE, ...) {
  args <- x$control_args
  funs <- vapply(args, is.function, logical(1))
  
  c(list(id = x$id, type = x$listener), args[!funs])
}

#' @export
format.handler <- function(x, ...) {
  control <- as.call(c(as.name(class(x)[1]), x$control_args))
  control_s <- paste0(deparse(control), collapse = "\n")
  
  paste0("<handler> ", x$id, "\n", control_s, "\n")
}

#' @export
#' @rdname handler
is.handler <- function(x) inherits(x, "handler")

#' @export
controls.handler <- function(x, session = NULL, ...) NULL

# Code to extract handlers -----------------------------------------------------

# Extract all handlers from a ggvis object. This shares a lot of code with
# control(), so probably need to extract out standard walk function.
handlers <- function(x) UseMethod("handlers")

#' @export
handlers.layer <- function(x) {
  t_handlers <- unname(lapply(x$data, handlers))
  p_handlers <- unname(lapply(x$props, handlers))
  c_handlers <- unname(lapply(x$children, handlers))
  
  compact(c(t_handlers, p_handlers, c_handlers, x$handlers))
}

#' @export
handlers.list <- function(x) {
  compact(lapply(x, handlers))
}
#' @export
handlers.ggvis_props <- handlers.list

#' @export
handlers.prop <- function(x) {
  handlers(x$dr)
}

#' @export
handlers.handler <- function(x) x

#' @export
handlers.transform <- function(x) {
  c(handlers.list(x), handlers.list(x$dots))
}

#' @export
handlers.default <- function(x) NULL

#' @export
handlers.transform_manip <- function(x) {
  handlers.list(x$inputs)
}

# Extract layers from handlers -------------------------------------------------

extract_layer <- function(x) UseMethod("extract_layer")

#' @export
extract_layer.handler <- function(x) NULL

#' @export
extract_layer.default <- function(x) NULL
