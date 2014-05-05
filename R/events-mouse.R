#' Display tooltips
#'
#' @param f A function that takes a single argument as input. This argument
#'   will be a list containing the data in the mark currently under the
#'   mouse. It should return a string containing HTML.
#' @export
#' @examples
#' \dontrun{
#' all_values <- function(x) {
#'   if(is.null(x)) return(NULL)
#'   paste0(names(x), ": ", format(x), collapse = "<br />")
#' }
#'
#' # Display tooltip when hovering over objects
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, size.hover := 400) %>%
#'   layer_points() %>%
#'   add_hover_tooltip(all_values)
#'
#' # Display tooltip when objects are clicked
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, size := 400) %>%
#'   layer_points() %>%
#'   add_click_tooltip(all_values)
#'
#'
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
add_hover_tooltip <- function(vis, f, id = rand_id()) {
  if (!is.function(f)) stop("f must be a function")

  connect <- function(session) {
    # FIXME: These should use the plot ID as the prefix
    # Shiny input IDs to to listen for
    mouse_out_id  <- paste0("ggvis_", id, "_mouse_out")
    mouse_over_id <- paste0("ggvis_", id, "_mouse_over")

    shiny::observe({
      session$input[[mouse_out_id]]
      hide_tooltip(session)
    })

    shiny::observe({
      hover <- session$input[[mouse_over_id]]

      if (is.null(hover$data)) {
        hide_tooltip(session)
        return()
      }

      html <- f(hover$data)

      show_tooltip(session,
        pagex = hover$pagex + 5,
        pagey = hover$pagey + 5,
        html = html
      )
    })
  }
  connector_label(connect) <- paste("hover", id)

  # This gets inserted into the Vega spec
  spec <- list(id = id, type = "hover")

  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)

  register_reactive(vis, broker)
}

#' @export
#' @rdname tooltip
add_click_tooltip <- function(vis, f, id = rand_id()) {
  if (!is.function(f)) stop("f must be a function")

  connect <- function(session) {
    # FIXME: These should use the plot ID as the prefix
    # Shiny input IDs to to listen for
    mouse_click_id  <- paste0("ggvis_", id, "_mouse_click")

    shiny::observe({
      click <- session$input[[mouse_click_id]]

      if (is.null(click$data)) {
        hide_tooltip(session)
        return()
      }

      html <- f(click$data)

      show_tooltip(session,
        pagex = click$pagex + 5,
        pagey = click$pagey + 5,
        html = html
      )
    })
  }
  connector_label(connect) <- paste("click", id)

  # This gets inserted into the Vega spec
  spec <- list(id = id, type = "click")

  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)

  register_reactive(vis, broker)
}

#' Send a message to the client to show or hide a tooltip
#'
#' @param session A Shiny session object.
#' @param pagex x position of the tooltip box on the page.
#' @param pagey y position of the tooltip box on the page.
#' @param html HTML to display in the tooltip box.
#'
#' @export
show_tooltip <- function(session, pagex = 0, pagey = 0, html = "") {
  ggvis_message(session, "show_tooltip",
    list(pagex = pagex, pagey = pagey, html = html))
}

#' @rdname show_tooltip
#' @export
hide_tooltip <- function(session) {
  ggvis_message(session, "hide_tooltip")
}
