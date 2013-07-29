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
    view_dynamic(gr$spec, gr$data_table)
  } else {
    view_static(gr$spec)
  }
}

#' Tools to save and view static specs.
#' 
#' These functions are mainly useful for testing.
#' 
#' @keywords internal
#' @export
save_spec <- function(x, path, ...) {
  gr <- gigvis_render(x, ...)
  
  json <- toJSON(gr$spec, pretty = TRUE)
  writeLines(json, path)
}

#' @importFrom RJSONIO fromJSON
#' @rdname save_spec
view_spec <- function(path, ...) {
  contents <- paste0(readLines(path), collapse = "\n")
  spec <- fromJSON(contents)
  view_static(spec)
}