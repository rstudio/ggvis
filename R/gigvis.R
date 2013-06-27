#' @export
#' @import assertthat
gigvis <- function(data = NULL, props = NULL, ..., dynamic = FALSE) {
  structure(
    node(data = data, props = props, ..., dynamic = dynamic),
    class = c("gigvis", "gigvis_node")
  )
}


#' @export
node <- function(..., data = NULL, props = NULL, scales = NULL,
                 dynamic = FALSE) {
  # assert_that(is.character(props), !is.null(names(props)))
  if (is.null(scales))  scales <- list()

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
  gv_filled <- gigvis_fill_tree(gv, parent = NULL, envir = envir)
  prerendered <- gigvis_prerender(gv_filled)

  if (gv$dynamic)
    view_dynamic(prerendered)
  else
    view_static(prerendered)
}
