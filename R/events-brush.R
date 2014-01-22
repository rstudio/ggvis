#' @include events-mouse.R
NULL

#' Event broker for brush events.
#'
#' @export
#' @importFrom methods setRefClass
Brush <- setRefClass("Brush", contains = "EventBroker",
  methods = list(
    brush_move = function() {
      "A reactive value that changes every time the brush is moved.
      The return should be ignored."

      listen_for("brush_move")
    }
  )
)

#' @export
#' @rdname tooltip
brush_tooltip <- function(f) {
  stopifnot(is.function(f))

  handler("brush_tooltip", "brush", list(f = f))
}

#' @export
as.reactive.brush_tooltip <- function(x, session = NULL, ...) {
  h <- Brush(session, id = x$id)

  obs <- observe({
    brush <- h$brush_move()
    if (is.null(brush$items) || length(brush$items) == 0) {
      hide_tooltip(session)
      return()
    }

    html <- x$control_args$f(brush$items)

    show_tooltip(session,
      pagex = brush$pagex2 + 5,
      pagey = brush$pagey1 + 5,
      html = html
    )
  })

  session$onSessionEnded(function() {
    obs$suspend()
  })

  reactive({ NULL })
}
