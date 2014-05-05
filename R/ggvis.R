#' Visualise a data set with a ggvis graphic.
#'
#' @param data A data object.
#' @param ... Property mappings.
#' @param env Environment in which to evaluate properties.
#' @import assertthat
#' @importFrom shiny reactive
#' @export
#' @examples
#' ggvis(mtcars, ~mpg, ~wt)
#' mtcars %>% ggvis(~mpg, ~wt)
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
#' @param ... Named visual properties.
#' @export
#' @examples
#' mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
#' mtcars %>% ggvis() %>% add_props(~wt, ~mpg) %>% layer_points()
#' mtcars %>% ggvis(~wt) %>% add_props(y = ~mpg) %>% layer_points()
add_props <- function(vis, ..., .props = NULL, inherit = NULL,
                      env = parent.frame()) {

  # Get value of inherit from inherit arg, then .props$inherit, then TRUE
  if (!is.null(.props)) inherit <- attr(.props, "inherit")
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
#' @export
#' @examples
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
#' NULL %>% ggvis(~mpg, ~wt) %>% add_data(mtcars) %>% layer_points()
add_data <- function(vis, data, name = deparse2(substitute(data))) {
  if (is.null(data)) return(vis)

  # Make sure data is reactive
  if (!shiny::is.reactive(data)) {
    static_data <- data
    data <- function() static_data
  }

  data <- add_data_id(data, name)
  vis$data[[get_data_id(data)]] <- data
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

  vis$marks <- c(vis$marks, list(
    mark(type, props = vis$cur_props, data = vis$cur_data))
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
#' @keywords internal
#' @export
add_scale <- function(vis, scale) {
  if (!is.ggvis(vis)) stop("Object to add legend to is not a ggvis object.")

  # Use the 'name' field as the name
  vis$scales[[scale$name]] <- scale
  vis
}

add_legend <- function(vis, legend) {
  if (!is.ggvis(vis)) stop("Object to add legend to is not a ggvis object.")

  vis$legends <- c(vis$legends, list(legend))
  vis
}

add_axis <- function(vis, axis) {
  if (!is.ggvis(vis)) stop("Object to add legend to is not a ggvis object.")

  vis$axes <- c(vis$axes, list(axis))
  vis
}

# If replace is TRUE, new options overwrite existing options; if FALSE, they don't.
add_options <- function(vis, options, replace = TRUE) {
  if (!is.ggvis(vis)) stop("Object to add legend to is not a ggvis object.")
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
  id <- paste0(get_data_id(parent_data), "/", name)

  if (shiny::is.reactive(parent_data) || any_apply(args, shiny::is.reactive)) {
    new_data <- reactive(transform(parent_data(), values(args)))
  } else {
    cache <- transform(parent_data(), args)
    new_data <- function() cache
  }

  new_data <- add_data_id(new_data, id)
  vis$data[[get_data_id(new_data)]] <- new_data
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
  # Add reactive id if needed
  if (is.null(reactive_id(reactive))) {
    reactive_id(reactive) <- paste0("reactive_", digest::digest(reactive, algo = "crc32"))
  }

  label <- reactive_id(reactive)

  # Don't add if already registered
  if (label %in% names(vis$reactives)) return(vis)

  vis$reactives[[label]] <- reactive
  vis
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
