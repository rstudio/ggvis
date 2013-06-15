#' Given a filled gigvis spec object, output a gigvis_prerender object
#'
#' @export
gigvis_prerender <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  structure(
    list(
      vega_spec = vega_spec(gv)
    ),
    class = "gigvis_prerender"
  )
}
