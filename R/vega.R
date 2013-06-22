#' Given a filled gigvis spec object, output a gigvis_prerender object
#'
#'
#' The gigvis_prerender object contains a Vega spec.
#'
#' @param envir The environment in which to evaluate the \code{data} parameter
#'   of the gigvis object.
#' @export
vega_spec <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  if (gv$dynamic) {
    mapped_vars <- gather_mapped_vars(gv)

    symbol_table <- attr(gv, "symbol_table")
    scales <- gather_scales(gv, symbol_table)

    # Convert data frames to vega format
    datasets <- lapply(names(symbol_table), function(name) {
      # Don't provide data now, just the name
      list(name = name)
    })

  } else {
    datasets <- gather_datasets(gv)
    props <- gather_props(gv)
    datasets <- apply_props(datasets, props)
    scales <- add_scales(gv)

    # Convert data frames to vega format
    datasets <- lapply(names(datasets), function(name) {
      vega_df(datasets[[name]], name = name)
    })
  }


  # These are key-values that only appear at the top level of the tree
  spec <- list(
    width = width,
    height = height,
    data = datasets,
    scales = scales,

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
  spec <- c(spec, vega_process_node(node = gv, envir = envir, scales = scales))

  if (gv$dynamic) {
    # Pass along the dataset expressions too.
    attr(spec, "datasets") <- symbol_table
  }

  spec
}


# Recursively traverse tree and collect all the data sets used - this currently
# sends all datasets to vega, even though internal nodes probably don't need
# to sent
gather_datasets <- function(node) {
  if (is.null(node$data_id))
    data_id <-NULL
  else
    data_id <- setNames(list(node$data_obj), node$data_id)

  if (is.null(node$children)) return(data_id)

  children <- unlist(lapply(node$children, gather_datasets), recursive = FALSE)
  all <- c(children, data_id)
  all[!duplicated(names(all))]
}

# Recursively traverse tree and collect all the variable props used, for each
# data set.
gather_props <- function(node) {
  # Create a list with an entry for this data_id, containing the props for the
  # data_id
  data_id <- node$data_id
  props <- list()
  if (!is.null(data_id)) {
    var_props <- Filter(is.variable, node$props)
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


# Apply properties to each data object in the datasets list, creating
# calculated columns and dropping unused columns.
apply_props <- function(datasets, props) {
  mapply(datasets, names(datasets), SIMPLIFY = FALSE,
    FUN = function(data_obj, name) {
      # Get/calculate columns
      cols <- lapply(props[[name]], prop_value, data = data_obj)
      names(cols) <- vapply(props[[name]], prop_name, character(1))

      as.data.frame(compact(cols))
    }
  )
}


# Recursively process nodes in the gigvis tree, and return corresponding vega
# tree.
#
# @param node A gigvis object node.
# @param envir Environment in which to evaluate \code{data}, to retrieve
#   the data object.
# @param scales A list of scale objects
vega_process_node <- function(node, envir, scales) {

  if (inherits(node, "mark")) {
    # Leaf nodes
    vega_node <- vega_mark(node, scales)

  } else if (inherits(node, "gigvis_node")) {
    # Non-leaf nodes (including root node)
    vega_node <- list(
      marks = lapply(
        node$children,
        FUN = vega_process_node,
        envir = envir,
        scales = scales
      )
    )

    # For non-root, non-leaf nodes, add in grouping
    if (!inherits(node, "gigvis")) {

      vega_node$type <- "group"

      # group nodes need a transform of some type to work. If there's no
      # operation to be done, use type="facet" and keys=NULL.
      if (is.null(node$split)) {
        facet_keys <- NULL
      } else {
        facet_keys <- paste("data", node$split, sep = ".")
      }

      vega_node$from <- list(
        data = node$data_id,
        transform = list(list(
          type = "facet",
          keys = facet_keys
        ))
      )
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
