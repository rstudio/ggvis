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
  # Any changes to default should happen at top-level
  x$cur_vis <- NULL

  if (length(x$marks) == 0) {
    x <- layer_guess(x)
  }

  data_props <- combine_data_props(x$marks)
  data_ids <- names(data_props)
  data_table <- x$data[data_ids]

  # Collapse each list of scale objects into one scale object.
  x <- collapse_scales(x)
  scale_data_table <- scale_domain_data(x)

  # Wrap each of the reactive data objects in another reactive which returns
  # only the columns that are actually used, and adds any calculated columns
  # that are used in the props.
  data_table <- active_props(data_table, data_props)

  # From an environment containing data_table objects, get static data for the
  # specified ids.
  static_datasets <- function(data_table, ids) {
    datasets <- lapply(ids, function(id) {
      data <- shiny::isolate(data_table[[id]]())
      as.vega(data, id)
    })
    unlist(datasets, recursive = FALSE)
  }

  datasets <- static_datasets(data_table, data_ids)
  scale_datasets <- static_datasets(scale_data_table, names(scale_data_table))
  check_scales_complete(x)

  # Each of these operations results in a more completely specified (and still
  # valid) ggvis object
  x <- add_missing_axes(x)
  x <- apply_axes_defaults(x)
  x <- add_missing_legends(x)
  x <- fortify_legends(x)
  x <- apply_legends_defaults(x)
  x <- add_default_options(x)

  spec <- list(
    data = c(datasets, scale_datasets),
    scales = lapply(unname(x$scales), as.vega),
    marks = lapply(x$marks, as.vega),
    legends = compact(lapply(x$legends, as.vega)),
    axes = compact(lapply(x$axes, as.vega)),
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

gather_scales <- function(x) {
  groups <- Filter(is.mark_group, x$marks)
  c(x$scales, unlist(pluck(groups, "scales"), recursive = FALSE))
}

#' @export
as.vega.mark_group <- function(x, ...) {
  this_scales <- vpluck(x$scales, "name", character(1))

  list(
    type = "group",
    properties = as.vega(x$props),
    from = list(data = data_id(x$data)),
    marks = lapply(x$marks, as.vega, in_group = TRUE),
    scales = lapply(unname(x$scales), as.vega),
    legends = lapply(x$legends, as.vega),
    axes = lapply(x$axes, as.vega)
  )
}


# Given a ggvis mark object, output a vega mark object
#' @export
as.vega.mark <- function(mark, in_group = FALSE) {
  data_id <- data_id(mark$data)

  # Pull out key from props, if present
  key <- mark$props$key
  mark$props$key <- NULL

  # Add the custom ggvis properties set for storing ggvis-specific information
  # in the Vega spec.
  properties <- as.vega(mark$props)
  properties$ggvis <- list()
  properties$ggvis$data <- list(value = data_id)

  group_vars <- dplyr::groups(shiny::isolate(mark$data()))
  if (!in_group && !is.null(group_vars)) {
    # FIXME: probably should go away and just use subvis

    # String representation of groups
    group_vars <- vapply(group_vars, deparse, character(1))

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
    m <- list(
      type = mark$type,
      properties = properties
    )
    if (!in_group) {
      # If mark inside group, inherits data from parent.
      m$from <- list(data = data_id)
    }
  }

  if (!is.null(key)) {
    m$key <- paste0("data.", safe_vega_var(prop_label(key)))
  }
  m
}

#' @export
as.vega.ggvis_props <- function(x, default_scales = NULL) {
  x <- prop_event_sets(x)

  # Given a list of property sets (enter, update, etc.), return appropriate
  # vega property set.
  vega_prop_set <- function(x) {
    if (empty(x)) return(NULL)

    props <- trim_prop_event(names(x))
    default_scales <- default_scales %||% propname_to_scale(props)
    Map(prop_vega, x, default_scales)
  }

  lapply(x, vega_prop_set)
}

#' @export
as.vega.ggvis_axis <- function(x) {
  if (isTRUE(x$hide)) return(NULL)

  if (empty(x$properties)) {
    x$properties <- NULL
  } else {
    x$properties <- as.vega(x$properties)
  }

  unclass(x)
}
#' @export
as.vega.ggvis_legend <- as.vega.ggvis_axis

#' @export
as.vega.data.frame <- function(x, name, ...) {
  # Figure out correct vega parsers for non-string columns
  parsers <- drop_nulls(lapply(x, vega_data_parser))

  list(list(
    name = name,
    format = list(
      type = "csv",
      parse = parsers
    ),
    values = to_csv(x)
  ))
}

#' @export
as.vega.grouped_df <- function(x, name, ...) {
  # Create a flat data set and add a transform-facet data set which uses the
  # flat data as a source.
  group_vars <- vapply(dplyr::groups(x), deparse, character(1))
  res <- as.vega(dplyr::ungroup(x), paste0(name, "_flat"), ...)

  res[[length(res) + 1]] <- list(
    name = name,
    source = paste0(name, "_flat"),
    transform = list(list(
      type = "treefacet",
      keys = as.list(paste0("data.", safe_vega_var(group_vars)))
    ))
  )

  res
}
