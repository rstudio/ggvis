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
  props <- gather_props(x)
  
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


# Recursively traverse tree and collect all the variable props used, for each
# data set.
gather_props <- function(node) {
  # Create a list with an entry for this data_id, containing the props for the
  # data_id
  data_id <- node$data_id
  props <- list()
  if (!is.null(data_id)) {
    # Get variables from props
    var_props <- Filter(is.variable, node$props)
    # Also get split vars from the data pipeline
    var_props <- c(var_props, split_vars(node$data))
    names(var_props) <- vapply(var_props, prop_name, character(1))
    props[[data_id]] <- var_props
  }

  if (is.null(node$children)) return(props)

  children <- unlist(lapply(node$children, gather_props), recursive = FALSE)
  all <- c(props, children)

  # Merge the properties for each data_id (there may be multiple entries for
  # each data_id)
  all_names <- unique(names(all))
  names(all_names) <- all_names
  lapply(all_names, function(name) {
    Reduce(merge_vectors, all[names(all) == name])
  })
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
