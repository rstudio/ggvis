#' Given a gigvis object, output a vega object
#'
#'
#' @param envir The environment in which to evaluate the \code{data} parameter
#'   of the gigvis object.
vega_spec <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  gv <- gigvis_fill_tree(gv)
  gv <- standardize_data(gv, envir)

  # These are key-values that only appear at the top level of the tree
  spec <- list(
    width = width,
    height = height,
    data = gather_datasets(gv),
    scales = vega_scales(gv$scales, gv$mapping, gv$data),

    axes = list(list(type = "x", scale = "x"), list(type = "y", scale = "y")),
    padding = c(
      top = padding[1],
      right = padding[2],
      bottom = padding[3],
      left = padding[4]
    )
  )

  # Now deal with keys that also appear in lower levels of the tree, and merge
  # them in to the spec.
  spec <- c(spec, vega_process_node(node = gv, envir = envir))

  spec
}


# Recursively traverse tree and collect all the data sets used. Returns a flat
# list of data sets, converted to vega data format.
gather_datasets <- function(node) {
  if (!is.null(node$data)) {
    dataset <- vega_df(node$data_std, name = node$data)
  } else {
    dataset <- NULL
  }

  # Generate flat list of datasets, joining this node's data with children's
  datasets <- c(
    list(dataset),
    unlist(lapply(node$children, gather_datasets), recursive = FALSE)
  )

  # Drop duplicate datasets, by checking for duplicated 'name' keys
  datanames <- vapply(datasets, FUN = `[[`, 'name', FUN.VALUE = character(1))
  datasets[duplicated(datanames)] <- NULL

  datasets
}


# Recursively process nodes in the gigvis tree, and return corresponding vega
# tree.
#
# @param node A gigvis object node.
# @param envir Environment in which to evaluate \code{data}, to retrieve
#   the data object.
vega_process_node <- function(node, envir) {

  if (inherits(node, "mark")) {
    # Leaf nodes
    vega_node <- vega_mark(node)

  } else if (inherits(node, "gigvis_node")) {
    # Non-leaf nodes (including root node)
    vega_node <- list(
      marks = lapply(
        node$children,
        FUN = vega_process_node,
        envir = envir
      )
    )

    # For non-root, non-leaf nodes, add in grouping
    if (!inherits(node, "gigvis")) {
      vega_node$type <- "group"
      vega_node$from <- list(data = node$data, keys = NULL)
    }
  }

  vega_node
}


vega_df <- function(x, name) {
  list(
    name = name,
    values = d3df(x)
  )
}

d3df <- function(x) {
  n <- nrow(x)
  lapply(seq_len(n), function(i) as.list(x[i, ]))
}


# Given a gigvis scales object, return a vega scales object.
vega_scales <- function(scales, mapping, data) {
  scales <- lapply(scales, function(s) {
    vega_scale(s, s$name, data)
  })

  unname(scales)
}


# Given a gigvis scale, domain (like 'x'), and name of data set, return a
# vega scale specification.
vega_scale <- function(scale, domain, data) {
  if (scale$name == "x") {
    range <- "width"
  } else if (scale$name == "y") {
    range <- "height"
  }

  list(
    name = scale$name,
    type = scale$type,
    domain = list(
      data = data,
      field = paste("data", domain, sep = ".")
    ),
    range = range,
    zero = FALSE,
    nice = TRUE
  )
}


# Given a gigvis mark object, output a vega mark object
vega_mark <- function(node) {

  # Generate the fields related to mappings (x, y, etc)
  # This assumes that the scale's name is the same as the 'name' field, which
  # is true now but might not be a good assumption in the long run.
  vega_mapping <- list()
  for (name in names(node$mapping)) {
    vega_mapping[[name]] <- list(
      field = paste("data", node$mapping[[name]], sep = "."),
      scale = name
    )
  }

  # TODO: Support other properties besides just stroke and fill
  list(
    type = vega_mark_type(node),
    from = list(data = node$data),
    properties = list(
      update = c(
        vega_mapping,
        list(
          stroke = list(value = node$stroke),
          fill = list(value = node$fill)
        )
      )
    )
  )
}



# Given a gigvis mark object, return the vega mark type
vega_mark_type <- function(mark) UseMethod("vega_mark_type")

#' @S3method vega_mark_type default
vega_mark_type.default <- function(mark) NULL

#' @S3method vega_mark_type mark_point
vega_mark_type.mark_point <- function(mark) "symbol"

#' @S3method vega_mark_type mark_line
vega_mark_type.mark_line <- function(mark) "line"
