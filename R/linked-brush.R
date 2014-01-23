#' @examples
#' lb <- linked_brush(keys = 1:nrow(mtcars))
#'
#' qvis(mtcars, ~disp, ~mpg, fill := lb$fill_prop()) + lb$brush_handler()
#'
#' ggvis(mtcars, props(x = ~disp, y = ~mpg, fill := )) + mark_point() +
#'   lb$brush_handler()

linked_brush <- function(keys, fill = "red") {
  LinkedBrush(keys = keys, fill = fill)
}

#' @importFrom shiny reactiveValues
LinkedBrush <- setRefClass("LinkedBrush",
  fields = c("keys", "fill", "rv", "brush"),
  methods = list(
    initialize = function(keys = NULL, fill = "red", ...) {
      stopifnot(is.character(fill), length(fill) == 1)

      initFields(keys = keys, fill = fill, ...)

      rv <<- reactiveValues()
      rv$keys <<- character()

      brush <<- Brush()
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
      handler("linked_brush", "brush") #, id = brush$id)
    },

    as_vega = function(session) {
      session$observe({
        rv$keys <<- session$input[[message_name(name)]]
      })
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

  if (is.null(x$brush$session))
    x$brush$session <- session

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
