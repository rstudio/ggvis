#' @examples
#' lb <- linked_brush(keys = 1:nrow(mtcars))
#' qvis(mtcars, ~disp, ~mpg, fill := lb$fill_prop(), size.brush := 400) + lb$brush_handler()
#'
#' ggvis(mtcars, props(x = ~disp, y = ~mpg, fill := lb$fill_prop())) +
#'   mark_point() +
#'   lb$brush_handler()

linked_brush <- function(keys, fill = "red") {
  LinkedBrush(keys = keys, fill = fill)
}

#' @importFrom shiny reactiveValues
LinkedBrush <- setRefClass("LinkedBrush",
  fields = c("keys", "fill", "rv", "brush", "id"),
  methods = list(
    initialize = function(keys = NULL, fill = "red", ...) {
      stopifnot(is.character(fill), length(fill) == 1)

      initFields(keys = keys, fill = fill, ...)

      id <<- rand_id()

      rv <<- reactiveValues()
      rv$keys <<- character()
    },

    selected_prop = function() {
      "Returns a delayed reactive suitable for use in props"
      reactive_proxy(rv, "keys", rep(FALSE, length(keys)),
        function(x) keys %in% rv$keys
      )
    },
    fill_prop = function() {
      "Returns a delayed reactive suitable for use in props"
      reactive_proxy(rv, "keys", rep("black", length(keys)),
        function(x) c("black", fill)[keys %in% rv$keys + 1]
      )
    },

    brush_handler = function() {
      handler("linked_brush", "brush", list(fill = fill), id = rand_id())
    }
  )
)

reactive_proxy <- function(rv, name, default, trans = identity) {
  structure(
    list(rv = rv, name = name, trans = trans, default = default, id = rand_id()),
    class = c("reactive_proxy", "input"))
}

#' @export
as.reactive.reactive_proxy <- function(x, session = NULL, ...) {
  if (is.null(session)) return(reactive(x$default))

  brush <- Brush(session, id = x$id)
  observe({
    moved <- brush$brush_move()
    print(moved)
    x$rv$keys <- moved$keys
  })

  reactive(x$trans(x$rv[[x$name]]))
}

#' @export
print.reactive_proxy <- function(x, ...) {
  cat("<reactive_proxy>", " ", x$name, "\n", sep = "")
}

#' @export
controls.reactive_proxy <- function(x, session = NULL, ...) {
  NULL
}

#' @export
extract_layer.linked_brush <- function(x, ...) {
  comps <- parse_components(..., drop_named = TRUE)

  props <- merge_props(
    props(x := ~x, y := ~y, width := ~width, height := ~height,
      fill := x$control_args$fill, fillOpacity := 0.2,
      stroke := x$control_args$fill, strokeOpacity := 0.6,
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

