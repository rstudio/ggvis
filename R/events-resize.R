#' @include events.R
NULL

#' Event broker for resize events.
#' 
#' The resize event broker is useful if you want your shiny app to respond
#' to resize events.
#' 
#' @export
#' @importFrom methods setRefClass
Resize <- setRefClass("Resize", contains = "EventBroker",
  methods = list(
    resize = function() {
      "A reactive value changed when the plot is resized."
      
      listen_for("resize")
    }
  )
)

#' @export
#' @examples
#' ggvis(mtcars, props(x = ~mpg, y = ~wt)) +
#'   mark_symbol() +
#'   resize()
resize <- function(f) {
  stopifnot(is.function(f))
  handler("resize", "resize", list(f = f))
}

#' @export
as.reactive.resize <- function(x, session = NULL, ...) {
  h <- Resize(session, id = x$id)

  obs <- observe({
    r <- h$resize()
    if (is.null(r)) return()

    x$control_args$f(r)
  })

  session$onSessionEnded(function() {
    obs$suspend()
  })

  reactive({ NULL })
}
