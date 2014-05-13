#' Handle mouse actions on marks.
#'
#' @param vis Visualisation to listen to.
#' @param on_click,on_mouse_over Callback function with arguments:
#'   \describe{
#'    \item{data}{A data frame with one row}
#'    \item{location}{A named list with components x and y}
#'    \item{session}{The session, used to communicate with the browser}
#'   }
#' @param on_mouse_out Callback function with argument:
#'   \describe{
#'    \item{session}{The session, used to communicate with the browser}
#'   }
#' @export
#' @examples
#' location <- function(location, ...) cat(location$x, "x", location$y, "\n")
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points() %>%
#'   handle_click(location)
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points() %>%
#'   handle_hover(function(...) cat("over\n"), function(...) cat("off\n"))
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points() %>%
#'   handle_hover(function(data, ...) str(data))
handle_click <- function(vis, on_click = NULL) {
  check_callback(on_click, c("data", "location", "session"))

  connect <- function(session, plot_id) {
    watch_mouse(session, paste0(plot_id, "_mouse_click"), on_click)
  }

  spec <- list(type = "click")
  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)
  register_reactive(vis, broker)
}

#' @rdname handle_click
#' @export
handle_hover <- function(vis, on_mouse_over = NULL, on_mouse_out = NULL) {
  if (!is.null(on_mouse_over))
    check_callback(on_mouse_over, c("data", "location", "session"))
  if (!is.null(on_mouse_out))
    check_callback(on_mouse_out, "session")

  connect <- function(session, plot_id) {
    watch_mouse(session, paste0(plot_id, "_mouse_over"), on_mouse_over)

    if (is.null(on_mouse_out)) return()
    shiny::observe({
      value <- session$input[[paste0(plot_id, "_mouse_out")]]
      if (is.null(value)) return()

      on_mouse_out(session = session)
    })
  }

  spec <- list(type = "hover")
  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)
  register_reactive(vis, broker)
}

watch_mouse <- function(session, id, fun) {
  shiny::observe({
    value <- session$input[[id]]
    if (is.null(value)) return()
    if (!is.list(value$data)) return() # axis ticks/labels etc

    df <- value$data
    class(df) <- "data.frame"
    attr(df, "row.names") <- .set_row_names(1L)

    fun(
      data = df,
      location = list(x = value$pagex, y = value$pagey),
      session = session
    )
  })
}
