#' # Grab the mean and standard deviations of brushed values
#' brushed_summary <- function(x) {
#'  if(is.null(x) || length(x) == 0) return(NULL)
#'  # Get the names of variables other than "key__"
#'  names <- setdiff(names(x[[1]]), "key__")
#'
#'  # Print out the mean and sd for each variable
#'  lines <- lapply(names, function(name) {
#'    vals <- vapply(x, `[[`, name, FUN.VALUE = numeric(1))
#'    paste0(name, ": ", round(mean(vals), 2), " (", round(sd(vals), 2), ")")
#'  })
#'
#'  paste0(lines, collapse = "<br />")
#' }
#'
#' # Display tooltip when objects are brushed
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, size.brush := 400) %>%
#'   layer_points() %>%
#'   add_brush_tooltip(brushed_summary)
#' }
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
