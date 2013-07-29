#' Given a filled gigvis spec object, output a gigvis_prerender object
#'
#'
#' The gigvis_prerender object contains a Vega spec.
#'
#' @param envir The environment in which to evaluate the \code{data} parameter
#'   of the gigvis object.
#' @export
vega_spec <- function(x, nodes, data_table,
                      width = 600, height = 400, padding = NULL,
                      envir = parent.frame()) {

  scales <- find_scales(x, nodes, data_table)
  legends <- vega_legends(scales)
  
  data_names <- ls(data_table, all = TRUE)
  if (x$dynamic) {
    datasets <- lapply(data_names, function(name) {
      # Don't provide data now, just the name
      list(name = name)
    })
  } else {
    datasets <- lapply(data_names, function(name) {
      data <- isolate(data_table[[name]]())
      list(
        name = name,
        values = d3df(data)
      )
    })
  }

  # These are key-values that only appear at the top level of the tree
  spec <- list(
    width = width,
    height = height,
    data = datasets,
    scales = scales,
    legends = legends,

    axes = list(list(type = "x", scale = "x"), list(type = "y", scale = "y"))
  )

  if (!is.null(padding)) {
    spec$padding <- c(
      top = padding[1],
      right = padding[2],
      bottom = padding[3],
      left = padding[4]
    )
  }
  
  spec$marks <- lapply(nodes, vega_mark, scales = scales)

  spec
}

# Convert a data object to a D3-structured data object.
# Numbers and strings stay the same type; everything else gets converted to
# strings with as.character().
d3df <- function(x) UseMethod("d3df")

#' @S3method d3df data.frame
d3df.data.frame <- function(x) {
  rows <- nrow(x)
  colnames <- setNames(names(x), names(x))

  x <- lapply(x, function(col) {
    if (is.numeric(col) || is.character(col))  col
    else  as.character(col)
  })

  lapply(seq_len(rows), function(i) {
    lapply(colnames, function(colname) {
      .subset2(.subset2(x, colname), i)
    })
  })
}

#' @S3method d3df split_df
d3df.split_df <- function(x) {
  unlist(lapply(x, d3df), recursive = FALSE)
}
