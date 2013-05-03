#' Given a gigvis object, output a vega object
#'
#'
#' @param envir The environment in which to evaluate the \code{data} parameter
#'   of the gigvis object.
#'
#' @importFrom RJSONIO toJSON
vega_spec <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  # These are key-values that only appear at the top level of the tree
  spec <- list(
    width = width,
    height = height,
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
  spec <- c(spec, vega_process_node(node = gv, state = list(), envir = envir))

  spec
}


# Recursively process nodes in the tree.
#
# The `state` argument should be a list containing the values of `data` and
# `mapping` at this level of the tree. This is necessary because the values
# can be set (or not) at each level of the tree; if they're not set, they're
# inherited from the previous level. This algorithm recursively traverses
# the tree, and needs to keep track of what the inherited state currently is.
#
# @param node A gigvis object node.
# @param state The inherited state of data and mapping at this level of the tree.
# @param envir Environment in which to evaluate \code{data}, to retrieve
#   the data object.
vega_process_node <- function(node, state, envir) {

  if (inherits(node, "gigvis")) {
    data <- eval(parse(text = node$data), envir)
    data <- list(vega_df(data, name = node$data))

    return(list(
      data = data,
      marks = lapply(
        node$children,
        FUN = vega_process_node,
        state = list(data = node$data, mapping = node$mapping),
        envir = envir)
    ))

  } else if (inherits(node, "mark")) {
    vega_mark(node, state$mapping, state$data)
  }
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
# Input:
# $ scales   :List of 2
#  ..$ x:List of 2
#  .. ..$ name: chr "x"
#  .. ..$ type: chr "linear"
#  ..$ y:List of 2
#  .. ..$ name: chr "y"
#  .. ..$ type: chr "linear"
# $ mapping  : Named chr [1:2] "wt" "mpg"
#  ..- attr(*, "names")= chr [1:2] "x" "y"
# $ data     : chr "mtcars"
#
# Output:
# $ scales :List of 2
#  ..$ :List of 6
#  .. ..$ name  : chr "x"
#  .. ..$ type  : chr "linear"
#  .. ..$ domain:List of 2
#  .. .. ..$ data : chr "mtcars"
#  .. .. ..$ field: chr "data.wt"
#  .. ..$ range : chr "width"
#  .. ..$ zero  : logi FALSE
#  .. ..$ nice  : logi TRUE
#  ..$ :List of 6
#  .. ..$ name  : chr "y"
#  .. ..$ type  : chr "linear"
#  .. ..$ domain:List of 2
#  .. .. ..$ data : chr "mtcars"
#  .. .. ..$ field: chr "data.mpg"
#  .. ..$ range : chr "height"
#  .. ..$ zero  : logi FALSE
#  .. ..$ nice  : logi TRUE
vega_scales <- function(scales, mapping, data) {
  # This assumes that the scale's name is the same as the 'name' field, which
  # is true now but might not be a good assumption in the long run.
  # (The 'name' field is matched up with the names in mapping.)
  lapply(names(scales), function(name) {
    vega_scale(scales[[name]], mapping[[name]], data)
  })
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
vega_mark <- function(mark, mapping, data) {

  if (inherits(mark, "mark_point"))  type = "symbol"

  # Generate the fields related to mappings (x, y, etc)
  # This assumes that the scale's name is the same as the 'name' field, which
  # is true now but might not be a good assumption in the long run.
  vega_mapping <- list()
  for (name in names(mapping)) {
    vega_mapping[[name]] <- list(
      field = paste("data", mapping[[name]], sep = "."),
      scale = name
    )
  }

  # TODO: Support other properties besides just stroke and fill
  list(
    type = type,
    from = list(data = data),
    properties = list(
      update = c(
        vega_mapping,
        list(
          stroke = list(value = mark$stroke),
          fill = list(value = mark$fill)
        )
      )
    )
  )
}
