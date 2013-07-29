#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ..., dynamic = FALSE, scales = NULL) {
  structure(
    list(
      data = as.pipeline(data), 
      props = props, 
      dynamic = dynamic,
      scales = scales,
      children = list(...)),
    class = c("gigvis", "gigvis_node")
  )
}


#' @export
node <- function(..., data = NULL, props = NULL) {
  stopifnot(is.null(scales) || is.scales(scales))
  if (is.null(scales))  scales <- scales()

  structure(
    list(
      data = as.pipeline(data),
      props = props,
      scales = scales,
      children = list(...),
      dynamic = dynamic
    ),
    class = "gigvis_node"
  )
}

#' @S3method print gigvis
print.gigvis <- function(gv, envir = parent.frame()) {
  gr <- gigvis_render(gv)

  if (gv$dynamic) {
    view_dynamic(gr)
  } else {
    view_static(gr)
  }
}
