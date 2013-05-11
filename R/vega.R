#' Given a gigvis object, output a vega object
#'
#'
#' @param envir The environment in which to evaluate the \code{data} parameter
#'   of the gigvis object.
vega_spec <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  gv <- gigvis_fill_tree(gv, parent = NULL, envir = envir)
  mapped_vars <- gather_mapped_vars(gv)
  datasets <- gather_datasets(gv)
  datasets <- prune_datasets_columns(datasets, mapped_vars)

  # Convert data frames to vega format
  datasets <- lapply(names(datasets), function(name) {
    vega_df(datasets[[name]], name = name)
  })

  # These are key-values that only appear at the top level of the tree
  spec <- list(
    width = width,
    height = height,
    data = datasets,
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
# list of data sets
gather_datasets <- function(node) {
  # Generate flat list of datasets, joining this node's data with children's
  datasets <-
    unlist(lapply(node$children, gather_datasets), recursive = FALSE)

  # Drop any duplicates (e.g., if two children have same data set)
  dups <- duplicated(names(datasets))
  if (any(dups)) {
    datasets <- datasets[!dups]
  }

  # Add this node's data set if not already present
  if (!(node$data %in% names(datasets))) {
    datasets[[node$data]] <- node$data_df
  }

  datasets <- drop_nulls(datasets)

  datasets
}


# Given a gigvis node, recursively traverse tree and collect all mapped
# variables for all data sets used. Returns a flat named list of vectors, where
# the name of each vector is the name of the data set, and the content of each
# vector are strings naming each column used.
gather_mapped_vars <- function(node) {

  # Find all of children's mapped variables
  all_mapped_vars <-
    unlist(lapply(node$children, gather_mapped_vars), recursive = FALSE)

  # Add current node's mapped variables to the list
  all_mapped_vars[[node$data]] <- unname(node$mapping)

  all_mapped_vars <- drop_nulls(all_mapped_vars)


  # Iterate over each data name, merging all entries that share the same name
  for (dataname in unique(names(all_mapped_vars))) {
    # Find all entries for this data frame
    matchidx <- names(all_mapped_vars) == dataname

    if (sum(matchidx) > 1) {
      # Append the vectors together, and drop duplicate entries
      mapped_vars <- Reduce(c, all_mapped_vars[matchidx], character(0))
      mapped_vars <- unique(mapped_vars)

      # Drop all existing data sets, and then add back the
      all_mapped_vars[matchidx] <- NULL
      all_mapped_vars[[dataname]] <- mapped_vars
    }
  }

  all_mapped_vars
}

# Given a named list of data frames and a corresponding named list of mapped
# variables for each data frame,
prune_datasets_columns <- function(datasets, mapped_vars) {
  if (sort(names(datasets)) != sort(names(mapped_vars))) {
    stop("Names of datasets do not match names of sets of mapped vars.")
  }

  for (name in names(datasets)) {
    datasets[[name]] <- datasets[[name]][, mapped_vars[[name]] ]
  }

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


# Given a gigvis scales object, a named vector of mappings, and the name of the
# source data, return a vega scales object.
vega_scales <- function(scales, mapping, data) {
  scales <- lapply(names(scales), function(name) {
    vega_scale(scales[[name]], mapping[[name]], data)
  })

  unname(scales)
}


# Given a gigvis scale, domain (the name of a source column, like 'mpg'), and
# name of data set, return a vega scale specification.
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
