#' @export
#' @rdname tooltip
add_brush_tooltip <- function(vis, f) {
  if (!is.function(f)) stop("f must be a function")

  connect <- function(session, plot_id) {
    # FIXME: These should use the plot ID as the prefix
    # Shiny input IDs to to listen for
    brush_move_id  <- paste0(plot_id, "_brush_move")

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
  connector_label(connect) <- "brush"

  spec <- list(type = "brush")

  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)

  vis <- register_reactive(vis, broker)

  vis <- layer_f(vis, function(v) {
    v <- add_data(v, data.frame(x = 0, y = 0, width = 0, height = 0),
                  "ggvis_brush", add_hash = FALSE)
    emit_rects(v, props(x := ~x, y := ~y,
                        width := ~width, height := ~height,
                        fill := "black", fillOpacity := 0.2,
                        stroke := "black", strokeOpacity := 0.6,
                        inherit = FALSE))
  })

  vis
}
