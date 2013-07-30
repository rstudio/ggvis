#' Coerce an gigvis object to a vega list.
#' 
#' This generic function powers the coercion of gigvis objects to vega
#' compatible data structures.
#' 
#' @param x an object to convert to vega
#' @return a list. When converted to JSON, will be the type of structure
#'   that vega expects.
as.vega <- function(x, ...) {
  UseMethod("as.vega", x)
}

#' @S3method as.vega gigvis
as.vega.gigvis <- function(x, nodes, data_table,
                      width = 600, height = 400, padding = NULL) {
  
  if (is.null(padding)) padding <- padding()

  data_names <- ls(data_table, all = TRUE)
  if (x$dynamic) {
    datasets <- lapply(data_names, function(name) {
      # Don't provide data now, just the name
      list(name = name)
    })
  } else {
    datasets <- lapply(data_names, function(name) {
      data <- isolate(data_table[[name]]())
      out <- as.vega(data)
      out$name <- name
      out
    })
  }
  
  scales <- find_scales(x, nodes, data_table)
  spec <- list(
    data = datasets,
    scales = scales,
    marks = lapply(nodes, as.vega),
    width = width,
    height = height,
    legends = vega_legends(scales),
    axes = vega_axes(scales),
    padding = as.vega(padding)
  )

  spec
}

# Given a gigvis mark object and set of scales, output a vega mark object
#' @S3method as.vega mark
as.vega.mark <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)
  check_mark_props(mark, names(props))
  
  # Convert each property to a Vega-structured property
  vega_props <- Map(prop_vega, props, prop_to_scale(names(props)))
  
  list(
    type = mark$type,
    properties = list(
      update = vega_props
    ),
    from = list(data = mark$pipeline_id)
  )
}


#' @S3method as.vega data.frame
as.vega.data.frame <- function(x) {
  list(values = df_to_json(x))
}

#' @S3method as.vega split_df
as.vega.split_df <- function(x) {
  list(
    format = "treejson",
    children = lapply(x, function(x) list(children = df_to_json(x)))
  )
}

