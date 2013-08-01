#' Create a gigvis object, or a node in a gigvis plot.
#'
#' @param data A data \code{\link{pipeline}}, or anything that can be coerced
#'   to a pipeline using \code{\link{as.pipeline}}
#' @param props a list of \code{\link{props}} defining default mark properties
#'   for this node and all its children.
#' @param ... children \code{node}s or \code{\link{marks}}.
#' @param dynamic if \code{TRUE}, this printing this plot will generate a 
#'   dynamic shiny app; if \code{FALSE}, creates a static html file. See
#'   also \code{\link{save_spec}} to just output the vega spec
#' @param scales a \code{\link{scales}} object, to override the default scales
#' @param axes a list of \code{\link{axis}} specifications
#' @param legends a list of \code{\link{axis}} specifications
#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ..., dynamic = FALSE, 
                   scales = NULL, axes = NULL, legends = NULL) {
  if (is.null(scales)) scales <- scales()
  stopifnot(is.scales(scales))
  
  vis <- structure(
    list(
      data = as.pipeline(data), 
      props = props, 
      dynamic = dynamic,
      scales = scales,
      axes = axes,
      legends = legends,
      children = list(...)),
    class = c("gigvis", "gigvis_node")
  )
  set_last_vis(vis)
  vis
}

#' @export
#' @rdname gigvis
is.gigvis <- function(x) inherits(x, "gigvis")

#' @export
#' @rdname gigvis
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
  set_last_vis(x)
  
  if (x$dynamic) {
    view_dynamic(x)
  } else {
    view_static(x)
  }
}

#' Tools to save and view static specs.
#' 
#' These functions are mainly useful for testing.
#' 
#' @param path location to save spec to, or load spec from
#' @param x a gigvis object
#' @param ... other arguments passed to \code{as.vega}
#' @keywords internal
#' @export
save_spec <- function(path, x = last_vis(), ...) {
  assert_that(is.gigvis(x), is.string(path))
  
  json <- toJSON(as.vega(x, ...), pretty = TRUE)
  writeLines(json, path)
}

#' @importFrom RJSONIO fromJSON
#' @rdname save_spec
view_spec <- function(path, ...) {
  contents <- paste0(readLines(path), collapse = "\n")
  spec <- fromJSON(contents)
  view_static(spec)
}
