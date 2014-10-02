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
#' # If you don't supply a layer, ggvis uses layer_guess() to guess at
#' # an appropriate type:
#' mtcars %>% ggvis(~mpg, ~wt)
#' mtcars %>% ggvis(~mpg, ~wt, fill = ~cyl)
#' mtcars %>% ggvis(~mpg, ~wt, fill := "red")
#' mtcars %>% ggvis(~mpg)
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
      cur_props = NULL,
      cur_vis = NULL
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

  vis$props[[length(vis$props) + 1]] <- both_props
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

  # If we're in a subvis, modify scale names to include prefix
  # FIXME: figure out how to avoid this in order to specify parent scales
  # Maybe some attribute? e.g. scale = parent("x")
  if (!is.null(vis$cur_vis)) {
    suffix <- paste0(vis$cur_vis, collapse = "-")
    props <- lapply(props, function(x) {
      if (identical(x$scale, FALSE)) return(x)
      x$scale <- paste0(x$scale, suffix)
      x
    })
  }

  vis <- add_data(vis, data, data_name)
  vis <- add_props(vis, .props = props)


  vis <- register_scales_from_props(vis, cur_props(vis))

  new_mark <- mark(type, props = cur_props(vis), data = vis$cur_data)
  vis <- append_ggvis(vis, "marks", new_mark)

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
  vis <- append_ggvis(vis, "scales", scale)
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
    empty <- NULL

    # First time computation is executed, it must succeed. That's used to
    # determine the specification of the data, and if an error occurs in
    # subequent run, that specification is sent and the error is printed to
    # the console
    new_data <- reactive({
      if (is.null(empty)) {
        out <- transform(parent_data(), values(args))
        empty <<- out[0, , drop = FALSE]
        out
      } else {
        tryCatch(
          transform(parent_data(), values(args)),
          error = function(e) {
            message("Error: ", e$message)
            data.frame
          }
        )
      }
    })

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
    reactive_id(reactive) <- rand_id("reactive_")
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
  names(props) <- trim_prop_event(names(props))

  # Get a reactive for each scaled prop
  data <- vis$cur_data

  add_scale_from_prop <- function(vis, prop) {
    # Automatically add label, unless it's blank or has a trailing '_'
    label <- prop_label(prop)
    if (label == "" || grepl("_$", label)) {
      label <- NULL
    }

    if (is.prop_band(prop)) {
      # band() requires points = FALSE
      vis <- add_scale(
        vis,
        ggvis_scale(property = propname_to_scale(prop$property),
          name = prop$scale, points = FALSE, label = label)
      )
      return(vis)
    }

    if (is.null(prop$value) || !prop_is_scaled(prop) || is.null(data)) {
      return(vis)
    }

    type <- vector_type(shiny::isolate(prop_value(prop, data())))
    domain <- reactive({
      data_range(prop_value(prop, data()))
    })
    # Flag to not register this reactive in the ggvis reactives list. This is
    # so that these reactives don't make is.dynamic() think that the plot is
    # dynamic.
    attr(domain, "register") <- FALSE

    # e.g. scale_quantitative, scale_nominal
    scale_fun <- match.fun(paste0("scale_", type))

    vis <- scale_fun(vis, property = prop$property, name = prop$scale,
                     label = label, domain = domain, override = FALSE)
    vis
  }

  # Add them to the vis
  for (i in seq_along(props)) {
    vis <- add_scale_from_prop(vis, props[[i]])
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

  json <- jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE, force = TRUE,
                           null = "null")
  cat(gsub("\t", " ", json), "\n", sep = "")

  invisible(vis)
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

  json <- jsonlite::toJSON(as.vega(x, ...), pretty = TRUE, auto_unbox = TRUE,
                           force = TRUE, null = "null")
  writeLines(json, path)
}

#' @rdname save_spec
view_spec <- function(path, ...) {
  contents <- paste0(readLines(path), collapse = "\n")
  spec <- jsonlite::fromJSON(contents)
  view_static(spec)
}

append_ggvis <- function(vis, field, x) {
  i <- vis$cur_vis
  if (length(i) == 0) {
    vis[[field]] <- c(vis[[field]], list(x))
  } else if (length(i) == 1) {
    vis$marks[[i]][[field]] <- c(vis$marks[[i]][[field]], list(x))
  } else if (length(i) == 2) {
    vis$marks[[i[1]]]$marks[[i[2]]][[field]] <-
      c(vis$marks[[i[1]]]$marks[[i[2]]][[field]], list(x))
  } else {
    stop(">3 levels deep? You must be crazy!", call. = FALSE)
  }

  vis
}
