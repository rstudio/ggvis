#' Coerce an ggvis object to a vega list.
#'
#' This generic function powers the coercion of ggvis objects to vega
#' compatible data structures.
#'
#' @param x an object to convert to vega
#' @return a list. When converted to JSON, will be the type of structure
#'   that vega expects.
#' @keywords internal
as.vega <- function(x, ...) {
  UseMethod("as.vega", x)
}

#' @method as.vega ggvis
#' @export
#' @rdname as.vega
#' @param session a session object from shiny
#' @param dynamic whether to generate dynamic or static spec
as.vega.ggvis <- function(x, session = NULL, dynamic = FALSE, ...) {

  if (length(x$marks) == 0) {
    stop("No marks on plot.", call. = FALSE)
  }

  data_ids <- extract_data_ids(x$marks)
  data_table <- x$data[data_ids]

  scale_info <- summarize_scale_infos_r(x$scale_info)
  scale_data_table <- scale_domain_data(scale_info)

  # Wrap each of the reactive data objects in another reactive which returns
  # only the columns that are actually used, and adds any calculated columns
  # that are used in the props.
  data_table <- active_props(data_table, x$marks)

  # From an environment containing data_table objects, get static data for the
  # specified ids.
  static_datasets <- function(data_table, ids) {
    datasets <- lapply(ids, function(id) {
      data <- shiny::isolate(data_table[[id]]())
      as.vega(data, id)
    })
    unlist(datasets, recursive = FALSE)
  }

  if (dynamic) {
    # Don't provide data now, just the name
    datasets <- lapply(data_ids, function(id) {
      list(name = id)
    })
    scale_datasets <- lapply(names(scale_data_table), function(id) {
      list(name = id)
    })

  } else {
    datasets <- static_datasets(data_table, data_ids)
    scale_datasets <- static_datasets(scale_data_table, ls(scale_data_table))
  }

  # Run all the reactives in the list
  scale_info_static <- shiny::isolate(lapply(scale_info, function(x) x()))

  # Each of these operations results in a more completely specified (and still
  # valid) ggvis object
  x <- add_default_scales(x, scale_info_static)
  x <- add_default_axes(x)
  x <- apply_axes_defaults(x, scale_info_static)
  x <- add_default_legends(x)
  x <- apply_legends_defaults(x)
  x <- add_default_options(x)

  spec <- list(
    data = c(datasets, scale_datasets),
    scales = unname(x$scales),
    marks = lapply(x$marks, as.vega),
    width = x$options$width,
    height = x$options$height,
    legends = lapply(x$legends, as.vega),
    axes = lapply(x$axes, as.vega),
    padding = as.vega(x$options$padding),
    ggvis_opts = x$options,
    handlers = if (dynamic) x$handlers
  )

  structure(
    spec,
    data_table = data_table,
    scale_data_table = scale_data_table,
    controls = x$controls,
    connectors = x$connectors
  )
}


# Given a list of layers, return a character vector of all data ID's used.
extract_data_ids <- function(layers) {
  data_ids <- vapply(layers,
    function(layer) data_id(layer$data),
    character(1)
  )
  unique(data_ids)
}


# Given a ggvis mark object, output a vega mark object
#' @export
as.vega.mark <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.

  # Pull out key from props, if present
  key <- mark$props$key
  mark$props$key <- NULL

  # Add the custom ggvis properties set for storing ggvis-specific information
  # in the Vega spec.
  properties <- as.vega(mark$props)
  properties$ggvis <- list()

  # FIXME: dispatch on the class of mark$data()
  split <- !is.null(dplyr::groups(shiny::isolate(mark$data())))
  if (split) {
    data_id <- paste0(data_id(mark$data), "_tree")
    properties$ggvis$data <- list(value = data_id)

    m <- list(
      type = "group",
      from = list(data = data_id),
      marks = list(
        list(
          type = mark$type,
          properties = properties
        )
      )
    )

  } else {
    data_id <- data_id(mark$data)
    properties$ggvis$data <- list(value = data_id)

    m <- list(
      type = mark$type,
      properties = properties,
      from = list(data = data_id)
    )
  }

  if (!is.null(key)) {
    m$key <- paste0("data.", prop_name(key))
  }
  m
}

#' @export
as.vega.ggvis_props <- function(x, default_scales = NULL) {
  x <- prop_sets(x)

  # Given a list of property sets (enter, update, etc.), return appropriate
  # vega property set.
  vega_prop_set <- function(x) {
    if (empty(x)) return(NULL)

    props <- trim_propset(names(x))
    default_scales <- default_scales %||% prop_to_scale(props)
    Map(prop_vega, x, default_scales)
  }

  lapply(x, vega_prop_set)
}

#' @export
as.vega.vega_axis <- function(x) {
  if (empty(x$properties)) {
    x$properties <- NULL
  } else {
    x$properties <- lapply(x$properties, as.vega)
  }

  unclass(x)
}
#' @export
as.vega.vega_legend <- as.vega.vega_axis

#' @export
as.vega.data.frame <- function(x, name, ...) {
  # For CSV output, we need to unescape periods, which were turned into \. by
  # prop_name().
  names(x) <- gsub("\\.", ".", names(x), fixed = TRUE)

  list(list(
    name = name,
    format = list(
      type = "csv",
      # Figure out correct vega parsers for non-string columns
      parse = unlist(lapply(x, vega_data_parser))
    ),
    values = to_csv(x)
  ))
}

#' @export
as.vega.grouped_df <- function(x, name, ...) {
  # FIXME: This is effectively the same as dlply - is there a better way?
  vars <- dplyr::groups(x)
  group_vals <- lapply(vars, function(var) x[[as.character(var)]] )
  split_data <- unname(split(x, group_vals, drop = TRUE))

  data <- lapply(split_data, function(x) list(children = df_to_d3json(x)))

  list(
    list(
      name = paste0(name, "_tree"),
      format = list(
        type = "treejson",
        # Figure out correct vega parsers for non-string columns
        parse = unlist(lapply(x, vega_data_parser))
      ),
      values = list(children = data)
     ),
    list(
      name = name,
      source = paste0(name, "_tree"),
      transform = list(list(type = "flatten"))
    )
  )
}

