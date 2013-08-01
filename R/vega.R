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

#' @method as.vega gigvis
#' @export
#' @rdname as.vega
#' @param width,height width and height of plot, in pixels
#' @param padding padding, as described by \code{\link{padding}}
as.vega.gigvis <- function(x, width = 600, height = 400, padding = NULL,
                           session = NULL) {
  if (is.null(padding)) padding <- padding()

  nodes <- flatten(x, session = session)
  data_table <- extract_data(nodes)
  data_table <- active_props(data_table, nodes)
  
  data_names <- ls(data_table, all.names = TRUE)
  if (x$dynamic) {
    datasets <- lapply(data_names, function(name) {
      # Don't provide data now, just the name
      list(name = name)
    })
  } else {
    datasets <- unlist(lapply(data_names, function(name) {
      data <- isolate(data_table[[name]]())
      as.vega(data, name)
    }), recursive = FALSE)
  }
  
  scales <- find_scales(x, nodes, data_table)
  axes <- add_default_axes(x$axes, scales)
  legends <- add_default_legends(x$legends, scales)
  
  spec <- list(
    data = datasets,
    scales = unname(scales),
    marks = lapply(nodes, as.vega),
    width = width,
    height = height,
    legends = lapply(legends, as.vega),
    axes = lapply(axes, as.vega),
    padding = as.vega(padding)
  )

  structure(spec, data_table = data_table)
}

# Given a gigvis mark object and set of scales, output a vega mark object
#' @S3method as.vega mark
as.vega.mark <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)
  check_mark_props(mark, names(props))
  
  # HW: It seems less than ideal to have to inspect the data here, but
  # I'm not sure how else we can figure it out.
  split <- is.split_df(isolate(mark$pipeline()))
  
  if (split) {
    list(
      type = "group",
      from = list(data = paste0(mark$pipeline_id, "_tree")),
      marks = list(
        list(
          type = mark$type,
          properties = list(update = as.vega(props))
        )
      )
    )
  } else {
    list(
      type = mark$type,
      properties = list(update = as.vega(props)),
      from = list(data = mark$pipeline_id)
    )
  }
  
}

#' @S3method as.vega gigvis_props
as.vega.gigvis_props <- function(x, default_scales = NULL) {
  if (empty(x)) return(NULL)
  
  default_scales <- default_scales %||% prop_to_scale(names(x))
  Map(prop_vega, x, default_scales)
}

#' @S3method as.vega vega_axis
as.vega.vega_axis <- function(x) {
  if (empty(x$properties)) {
    x$properties <- NULL
  } else {
    x$properties <- lapply(x$properties, as.vega)
  }
  
  unclass(x)
}
#' @S3method as.vega vega_legend
as.vega.vega_legend <- as.vega.vega_axis

#' @S3method as.vega data.frame
as.vega.data.frame <- function(x, name, ...) {
  list(list(
    name = name,
    values = df_to_json(x)
  ))
}

#' @S3method as.vega split_df
as.vega.split_df <- function(x, name, ...) {
  data <- lapply(x, function(x) list(children = df_to_json(x)))
  
  list(
    list(
      name = paste0(name, "_tree"),
      format = list(type = "treejson"),
      values = list(children = data)
    ),
    list(
      name = name,
      source = paste0(name, "_tree"),
      transform = list(list(type = "flatten"))
    )
  )
}

