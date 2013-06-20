#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ..., dynamic = FALSE) {
  structure(
    node(data = data, props = props, fill_defaults = TRUE, ...,
      dynamic = dynamic),
    class = c("gigvis", "gigvis_node")
  )
}


#' @export
node <- function(..., data = NULL, props = NULL, scales = NULL,
                 fill_defaults = FALSE, dynamic = NULL) {
  # data is a data object, or a pipeline
  # props is a named character vector, permissible names are properties
  #   that vega understands
  # scales is a list of scale objects

  # assert_that(is.character(props), !is.null(names(props)))

  if (fill_defaults) {

    if (is.null(scales))  scales <- list()

    # For each props, add a scale if necessary
    for (name in names(props)) {
      if (is.null(scales[[name]])) {
        scales[[name]] <- scale(name)
      }
    }
  }

  children <- list(...)

  node <- list(
    data = as.pipeline(data),
    props = props,
    scales = scales,
    children = children
  )

  if (!is.null(dynamic))
    node$dynamic <- dynamic

  structure(
    node,
    class = "gigvis_node"
  )
}

#' @S3method print gigvis
print.gigvis <- function(gv, envir = parent.frame()) {
  gv_filled <- gigvis_fill_tree(gv, parent = NULL, envir = envir)
  prerendered <- gigvis_prerender(gv_filled)

  if (gv$dynamic)
    view_dynamic(prerendered)
  else
    view_static(prerendered)


}
