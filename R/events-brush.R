#' @export
#' @rdname tooltip
add_brush_tooltip <- function(vis, f, id = rand_id()) {
  if (!is.function(f)) stop("f must be a function")

  connect <- function(session) {
    # FIXME: These should use the plot ID as the prefix
    # Shiny input IDs to to listen for
    brush_move_id  <- paste0("ggvis_", id, "_brush_move")

    shiny::observe({
      brush <- session$input[[brush_move_id]]
      if (is.null(brush$items) || length(brush$items) == 0) {
        hide_tooltip(session)
        return()
      }

      html <- f(brush$items)

      show_tooltip(session,
        pagex = brush$pagex2 + 5,
        pagey = brush$pagey1 + 5,
        html = html
      )
    })
  }

  spec <- list(id = id, type = "brush")

  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)

  vis <- register_reactive(vis, broker)


  # FIXME: the data needs have id = "ggvis_brush" for Vega. Also, should have
  #   0 width and height
  dummy_rect <- data.frame(x = 0, y = 0, width = 100, height = 100)

  vis <- layer_rects(vis, x := ~x, y := ~y,
                     width := ~width, height := ~height,
                     fill := "black", fillOpacity := 0.2,
                     stroke := "black", strokeOpacity := 0.6,
                     inherit = FALSE, data = dummy_rect)
  vis
}
