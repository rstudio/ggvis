#' @include events-mouse.R
NULL

#' Event broker for brush events.
#'
#' @export
#' @importFrom methods setRefClass
Brush <- setRefClass("Brush", contains = "EventBroker",
  methods = list(
    brush_move = function() {
      "A reactive value that changes every time the brush is resized or moved.
      Returns a list containing:
        * plot_id: The ID of the ggvis plot.
        * x1, x2, y1, y2: Pixel coordinates of the brushed region, relative to
          plotting area.
        * pagex1, pagex2, pagey1, pagey2: Pixel coordinates of brushed region,
          relative to web page.
        * items: a list with one item per mark. Each item is another list
          containing one entry for each variable in the data, as well as a field
          named `key__`."

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

  obs <- shiny::observe({
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

  shiny::reactive({ NULL })
}

#' @export
extract_layer.brush_tooltip <- function(x, ...) {
  comps <- parse_components(..., drop_named = TRUE)

  props <- merge_props(
    props(x := ~x, y := ~y, width := ~width, height := ~height,
          fill := "black", fillOpacity := 0.2,
          stroke := "black", strokeOpacity := 0.6,
          inherit = FALSE),
    comps$props
  )

  mark_rect(
    props,
    data = pipeline(
      data.frame(x = 0, y = 0, width = 0, height = 0),
      .id = "ggvis_brush"
    )
  )
}
