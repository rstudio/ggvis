#' Visualise a data set with a ggvis graphic.
#'
#' \code{ggvis} is used to turn a dataset into a visualisation, setting up
#' default mappings between variables in the dataset and visual properties.
#' Nothing will be displayed until you add additional layers.
#'
#' @param data A data object.
#' @param ... Property mappings. If not named, the first two mappings are
#'   taken to be \code{x} and \code{y}. Common properties are \code{x},
#'   \code{y}, \code{stroke}, \code{fill}, \code{opacity}, \code{shape}
#' @param env Environment in which to evaluate properties.
#' @import assertthat
#' @importFrom shiny reactive
#' @export
#' @examples
#' \dontrun{
#' ggvis(mtcars, ~mpg, ~wt)
#' # Throws an error because there is nothing to show.
#' }
#'
#' # ggvis has a functional interface: every ggvis function takes a ggvis
#' # an input and returns a modified ggvis as output.
#' layer_points(ggvis(mtcars, ~mpg, ~wt))
#'
#' # To make working with this interface more natural, ggvis imports the
#' # pipe operator from magrittr. x %>% f(y) is equivalent to f(x, y) so
#' # we can rewrite the previous command as
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
#'
#' # For more complicated plots, add a line break after %>%
#' mtcars %>%
#'   ggvis(~mpg, ~wt) %>%
#'   layer_points() %>%
#'   layer_smooths()
ggvis <- function(data = NULL, ..., env = parent.frame()) {
  vis <- structure(
    list(
      marks = list(),
      data = list(),
      props = list(),
      reactives = list(),
      scales = list(),
      axes = list(),
      legends = list(),
      controls = list(),
      connectors = list(),
      handlers = list(),
      options = list(),
      cur_data = NULL,
      cur_props = NULL
    ),
    class = "ggvis"
  )

  vis <- add_data(vis, data, deparse2(substitute(data)))
  vis <- add_props(vis, ..., env = env)
  vis
}


#' Add visual properties to a visualisation
#'
#' @param vis Visualisation to modify.
#' @inheritParams props
#' @export
#' @examples
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
#' mtcars %>% ggvis() %>% add_props(~wt, ~mpg) %>% layer_points()
#' mtcars %>% ggvis(~wt) %>% add_props(y = ~mpg) %>% layer_points()
add_props <- function(vis, ..., .props = NULL, inherit = NULL,
                      env = parent.frame()) {

  # Get value of inherit from inherit arg, then .props$inherit, then TRUE
  if (!is.null(.props)) inherit <- attr(.props, "inherit", TRUE)
  inherit <- inherit %||% TRUE

  new_props <- props(..., .props = .props, inherit = inherit, env = env)
  both_props <- merge_props(cur_props(vis), new_props)

  vis$props[[props_id(props)]] <- both_props
  vis$cur_props <- both_props

  vis <- register_reactives(vis, extract_reactives(both_props))
  vis
}

#' Add dataset to a visualisation
#'
#' @param vis Visualisation to modify.
#' @param data Data set to add.
#' @param name Data of data - optional, but helps produce informative
#'  error messages.
#' @param add_suffix Should a unique suffix be added to the data object's ID?
#'   This should only be FALSE when the spec requires a data set with a
#'    specific name.
#' @export
#' @examples
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
#' NULL %>% ggvis(~mpg, ~wt) %>% add_data(mtcars) %>% layer_points()
add_data <- function(vis, data, name = deparse2(substitute(data)),
                     add_suffix = TRUE) {
  if (is.null(data)) return(vis)

  # Make sure data is reactive
  if (!shiny::is.reactive(data)) {
    static_data <- data
    data <- function() static_data
  }

  if (add_suffix) name <- paste0(name, length(vis$data))
  data_id(data) <- name
  vis$data[[name]] <- data
  vis$cur_data <- data

  vis
}

#' Is an object a ggvis object?
#'
#' @export
#' @param x an object to test
#' @keywords internal
is.ggvis <- function(x) inherits(x, "ggvis")


# Add a mark to a ggvis object.
add_mark <- function(vis, type = NULL, props = NULL, data = NULL,
                     data_name = "unnamed_data") {

  # Save current data
  old_data <- vis$cur_data
  old_props <- vis$cur_props

  vis <- add_data(vis, data, data_name)
  vis <- add_props(vis, .props = props)
  vis <- register_scales_from_props(vis, cur_props(vis))

  vis$marks <- c(vis$marks, list(
    mark(type, props = cur_props(vis), data = vis$cur_data))
  )

  # Restore old data
  vis$cur_data <- old_data
  vis$cur_props <- old_props
  vis
}

#' Add arbitrary scales to ggvis.
#'
#' @param vis Visualisation to modify.
#' @param scale Scale object
#' @param domain Either a vector with static values for the domain, or
#'   a reactive that returns a such a vector.
#' @param data_domain Should the domain be controlled by a data set which is
#'   added to the spec? Should only be set to FALSE in special cases.
#' @keywords internal
#' @export
add_scale <- function(vis, scale, data_domain = TRUE) {
  if (data_domain && shiny::is.reactive(scale$domain)) {
    vis <- register_reactive(vis, scale$domain)
  }

  vis$scales[[scale$name]] <- c(vis$scales[[scale$name]], list(scale))
  vis
}

add_legend <- function(vis, legend) {
  vis$legends <- c(vis$legends, list(legend))
  vis
}

add_axis <- function(vis, axis) {
  vis$axes <- c(vis$axes, list(axis))
  vis
}

# If replace is TRUE, new options overwrite existing options; if FALSE, they don't.
add_options <- function(vis, options, replace = TRUE) {
  if (replace) {
    vis$options <- merge_vectors(vis$options, options)
  } else {
    vis$options <- merge_vectors(options, vis$options)
  }
  vis
}

register_computation <- function(vis, args, name, transform = NULL) {
  vis <- register_reactives(vis, args)

  if (is.null(transform)) return(vis)

  parent_data <- vis$cur_data
  # For the ID, append to the parent's ID, along with a unique number.
  id <- paste0(data_id(parent_data), "/", name, length(vis$data))

  if (shiny::is.reactive(parent_data) || any_apply(args, shiny::is.reactive)) {
    new_data <- reactive(transform(parent_data(), values(args)))
  } else {
    cache <- transform(parent_data(), args)
    new_data <- function() cache
  }

  data_id(new_data) <- id
  vis$data[[id]] <- new_data
  vis$cur_data <- new_data

  vis
}

# Register a list of reactives in the ggvis object's reactives list
# @param vis A ggvis object.
# @param reactives A list of reactives.
register_reactives <- function(vis, reactives = NULL) {
  # Drop any objects from the 'reactives' list which aren't actually reactive
  reactives <- reactives[vapply(reactives, shiny::is.reactive, logical(1))]

  for (reactive in reactives) {
    vis <- register_reactive(vis, reactive)
  }
  vis
}

register_reactive <- function(vis, reactive) {
  # Some reactives are marked so that they're not registered
  if (identical(attr(reactive, "register"), FALSE)) return(vis)

  # Add reactive id if needed
  if (is.null(reactive_id(reactive))) {
    reactive_id(reactive) <- paste0("reactive_", digest::digest(reactive, algo = "crc32"))
  }

  label <- reactive_id(reactive)

  # Don't add if already registered
  if (label %in% names(vis$reactives)) return(vis)

  vis$reactives[[label]] <- reactive

  # If it's a broker, add controls, connector, and spec as needed
  if (is.broker(reactive)) {
    broker <- attr(reactive, "broker", TRUE)

    vis <- register_controls(vis, broker$controls)
    vis <- register_connector(vis, broker$connect)
    vis <- register_handler(vis, broker$spec)
  }

  vis
}

# Given a set of props, register a scale for each one.
register_scales_from_props <- function(vis, props) {
  # Strip off .update, .enter, etc.
  names(props) <- trim_propset(names(props))

  # Get a reactive for each scaled prop
  data <- vis$cur_data

  add_scale_from_prop <- function(vis, prop_name, prop) {
    if (!prop_is_scaled(prop) || is.null(data)) {
      return(vis)
    }

    # If we can get a valid scale name (like "x") from the prop, then use that
    # for the property and name. If we get something like "blah", then use that
    # for the name, but use prop_name for the property.
    property <- prop_scale(prop, default_scale = propname_to_scale(prop_name))
    if (property %in% valid_scales) {
      name <- property
    } else {
      property <- prop_name
      name <- prop$scale
    }

    type <- vector_type(shiny::isolate(prop_value(prop, data())))
    domain <- reactive({
      data_range(prop_value(prop, data()))
    })
    # Flag to not register this reactive in the ggvis reactives list. This is
    # so that these reactives don't make is.dynamic() think that the plot is
    # dynamic.
    attr(domain, "register") <- FALSE

    # e.g. scale_quantitative_int, scale_nominal_int
    scale_fun <- match.fun(paste0("scale_", type, "_int"))

    vis <- scale_fun(vis, property = property, name = name, domain = domain)
    vis
  }

  # Add them to the vis
  for (i in seq_along(props)) {
    vis <- add_scale_from_prop(vis, names(props)[i], props[[i]])
  }
  vis
}

# Takes a list of controls
register_controls <- function(vis, controls) {
  if (empty(controls)) return(vis)

  # If passed a bare control, wrap it into a list
  if (inherits(controls, "shiny.tag")) {
    controls <- list(controls)
  }
  vis$controls <- c(vis$controls, controls)
  vis
}

register_connector <- function(vis, connector) {
  vis$connectors <- c(vis$connectors, connector)
  vis
}

register_handler <- function(vis, handler) {
  if(empty(handler)) return(vis)

  vis$handlers <- c(vis$handlers, list(handler))
  vis
}

#' Print out the vega plot specification
#'
#' @param vis Visualisation to print
#' @param pieces Optional, a character or numeric vector used to
#'   pull out selected pieces of the spec
#' @export
#' @examples
#' base <- mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
#' base %>% show_spec()
#' base %>% show_spec("scales")
show_spec <- function(vis, pieces = NULL) {
  out <- as.vega(vis, dynamic = FALSE)

  if (!is.null(pieces)) {
    out <- out[pieces]
  }

  json <- RJSONIO::toJSON(out, pretty = TRUE)
  cat(gsub("\t", " ", json), "\n", sep = "")

  invisible()
}

#' Tools to save and view static specs.
#'
#' These functions are mainly useful for testing.
#'
#' @param path location to save spec to, or load spec from
#' @param x a ggvis object
#' @param ... other arguments passed to \code{as.vega}
#' @keywords internal
#' @export
save_spec <- function(x, path, ...) {
  assert_that(is.ggvis(x), is.string(path))

  json <- RJSONIO::toJSON(as.vega(x, ...), pretty = TRUE)
  writeLines(json, path)
}

#' @rdname save_spec
view_spec <- function(path, ...) {
  contents <- paste0(readLines(path), collapse = "\n")
  spec <- RJSONIO::fromJSON(contents)
  view_static(spec)
}
