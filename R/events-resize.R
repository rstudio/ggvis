#' An interactive input bound to resize events.
#'
#' @param f A function which is called each time the plot area is resized.
#'
#' @export
#' @examples
#' \dontrun{
#' # This example just prints out the current dimensions to the console
#' print_info <- function(x) {
#'   str(x)
#' }
#'
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points() %>% add_resize(print_info)
#' }
add_resize <- function(vis, f, id = rand_id()) {
  if (!is.function(f)) stop("f must be a function")

  connect <- function(session) {
    resize_id <- paste0("ggvis_", id, "_resize")

    shiny::observe({
      r <- session$input[[resize_id]]
      if (is.null(r)) return()

      f(r)
    })
  }
  connector_label(connect) <- paste("resize", id)

  # This gets inserted into the Vega spec
  spec <- list(id = id, type = "resize")
  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)

  register_reactive(vis, broker)
}
