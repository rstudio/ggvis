#' @export
#' @export
gigvis <- function(data = NULL, mapping = NULL, ..., dynamic = FALSE) {
  structure(
    node(data = data, mapping = mapping, fill_defaults = TRUE, ...,
      dynamic = dynamic),
    class = c("gigvis", "gigvis_node")
  )
}



#' @import assertthat


#' @export
node <- function(..., data = NULL, mapping = NULL, transform = NULL,
                 scales = NULL, split = NULL, fill_defaults = FALSE,
                 dynamic = NULL) {
  # data is a string
  # mapping is a named character vector, permissible names are properties
  #   that vega understands
  # transform is a transform object
  # scales is a list of scale objects
  # split is a spitter object

  # assert_that(is.character(mapping), !is.null(names(mapping)))

  if (fill_defaults) {

    if (is.null(scales))  scales <- list()

    # For each mapping, add a scale if necessary
    for (name in names(mapping)) {
      if (is.null(scales[[name]])) {
        scales[[name]] <- scale(name)
      }
    }
  }

  children <- list(...)

  node <- list(
    data = data,
    mapping = mapping,
    transform = transform,
    scales = scales,
    split = split,
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
