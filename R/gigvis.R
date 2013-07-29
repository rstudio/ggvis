#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ..., dynamic = FALSE, 
                   scales = NULL) {
  if (is.null(scales)) scales <- scales()
  stopifnot(is.scales(scales))
  
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
  structure(
    list(
      data = as.pipeline(data),
      props = props,
      children = list(...)
    ),
    class = "gigvis_node"
  )
}

#' @S3method print gigvis
print.gigvis <- function(x, ...) {
  gr <- gigvis_render(x)

  if (x$dynamic) {
    view_dynamic(gr)
  } else {
    view_static(gr)
  }
}
