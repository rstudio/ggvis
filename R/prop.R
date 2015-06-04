#' Create a property.
#'
#' Properties are used to describe the visual properties of \link{marks}.
#' You create a single property defintion with \code{prop}, and manage
#' sets of named properties with \code{\link{props}} (which also provides
#' shortcuts for creating the most common kind of properties)
#'
#' @param property A property, like "x", "x2", "y", "fill", and so on.
#' @param x The value of the property. This can be an atomic vector
#'   (a constant), a name or quoted call (a variable), a single-sided
#'   formula (a constant or variable depending on its contents), or a delayed
#'   reactive (which can be either variable or constant).
#' @param scale If \code{NULL}, automatically determine behavior by the kind of
#'   value (constant, variable, or reactive).
#'   If \code{TRUE} use the default scale associated with property.
#'   If \code{FALSE}, do not scale the value.
#'   Otherwise supply a string to select a custom scale.
#'   If \code{x} is an interactive input, then this defaults to the scale
#'   parameter of the input.
#' @param offset,mult Additive and multiplicate pixel offset used to adjust
#'   scaled values. These are useful if you want to place labels offset from
#'   points.
#' @param env If \code{x} is a quoted call this provides the environment in
#'   which to look for variables not in the data. You should not need this in
#'   ordinary operation.
#' @param event An event to which this property applies. One of "update",
#'   "enter", "exit", "hover", "brush".
#' @param label A label for this prop to use for reporting errors.
#' @seealso \code{\link{props}} to manage multiple properties and to
#'   succintly create the most common types.
#' @export
#' @examples
#' prop("x", 1)
#' prop("x", ~1)
#' prop("fill", quote(cyl))
#' prop("fill", ~cyl)
#' prop("x", input_slider(0, 100))
#'
#' # If you have a variable name as a string
#' var <- "cyl"
#' prop("x", as.name(var))
#'
#' # Use a custom scale
#' prop("y", quote(cyl), scale = "y-2")
#'
#' # Don't scale variable (i.e. it already makes sense in the visual space)
#' prop("fill", ~colour, scale = FALSE)
#'
#' # Use a constant, but scaled
#' prop("x", 5, scale = TRUE)
#'
#' # Use other events
#' prop("y", quote(cyl), scale = "y-2")
#'
prop <- function(property, x, scale = NULL, offset = NULL, mult = NULL,
                 env = parent.frame(), event = NULL, label = NULL) {

  if (missing(property)) stop("Property required for prop().")
  if (!is.character(property)) {
    stop("The interface to the prop() function has changed. The first argument ",
      "must be the name of a property, like 'x', 'y', or 'fill'. Instead of ",
      "props(x = prop(1)), use props(prop(\"x\", 1)). See ?prop and ?props.")
  }
  if (missing(x)) stop("Value required for prop().")
  if (property != "key" && is.null(event)) event <- "update"

  p <- new_prop(x, property, scale, offset, mult, env, event, label)

  if (p$property == "key") {
    if (!is.null(p$event)) stop("key prop cannot have an event.")
    if (!is.null(p$scale)) stop("key prop cannot have a scale.")
    if (is.prop_constant(p)) stop("key prop cannot be constant.")
  }

  p
}

#' Create new prop object
#'
#' The resulting object has the following fields:
#'
#' \itemize{
#'   \item property The name of a visual property, like "x", "x2", "width", "y",
#'     "fill".
#'   \item value A value. Can be a constant, reactive, or quoted expression.
#'   \item scale A string with name of a scale. Typically something like "x",
#'     "y", "fill", but can also be a custom name like "foo".
#'   \item offset Additive pixel offset used to adjust scaled values.
#'   \item mult Multiplicative pixel offset used to adjust scaled values.
#'   \item event A event like "update", "enter", "exit", "hover", "brush".
#'   \item env An environment in which to evaluate a variable or reactive value.
#' }
#' @keywords internal
new_prop <- function(x, property, scale, offset, mult, env, event, label) {
  UseMethod("new_prop")
}

#' @export
new_prop.prop <- function(x, ...) x

#' @export
new_prop.default <- function(x, property, scale, offset, mult, env, event,
                             label) {
  if (!is.atomic(x)) stop("Unknown input to prop: ", label)
  # If we got here, it's constant

  # strokeDash is the one property that can take an array
  if (property != "strokeDash") assert_that(length(x) == 1)

  structure(
    list(
      property = property,
      value = x,
      scale = decide_scale(scale %||% FALSE, property),
      offset = offset,
      mult = mult,
      event = event,
      env = NULL
    ),
    class = c("prop_constant", "prop")
  )
}

#' @export
new_prop.reactive <- function(x, property, scale, offset, mult, env, event,
                              label) {

  if (is.null(reactive_id(x))) {
    reactive_id(x) <- rand_id("reactive_")
  }

  structure(
    list(
      property = property,
      value = x,
      scale =  decide_scale(scale %||% FALSE, property),
      offset = offset,
      mult = mult,
      event = event,
      env = NULL
    ),
    class = c("prop_reactive", "prop")
  )
}

#' @export
new_prop.call <- function(x, property, scale, offset, mult, env, event,
                          label) {
  structure(
    list(
      property = property,
      value = x,
      scale = decide_scale(scale %||% TRUE, property),
      offset = offset,
      mult = mult,
      event = event,
      env = env
    ),
    class = c("prop_variable", "prop")
  )
}

#' @export
new_prop.name <- new_prop.call

#' @export
new_prop.formula <- function(x, property, scale, offset, mult, env, event,
                             label) {
  if (length(x) != 2) stop("Formulas must be single sided")
  new_prop.call(x[[2]], property, scale, offset, mult, environment(x),
                event, label)
}

# Given a value for scale and a property, return a string with the name of the
# scale. scale can be NULL, TRUE, FALSE, or a string.
decide_scale <- function(scale, property) {
  if (isTRUE(scale)) {
    propname_to_scale(trim_prop_event(property))
  } else if (identical(scale, FALSE)) {
    NULL
  } else {
    scale
  }
}


#' @export
#' @rdname prop
is.prop <- function(x) inherits(x, "prop")
#' @export
#' @rdname prop
is.prop_constant <- function(x) inherits(x, "prop_constant")
#' @export
#' @rdname prop
is.prop_variable <- function(x) inherits(x, "prop_variable")
#' @export
#' @rdname prop
is.prop_reactive <- function(x) inherits(x, "prop_reactive")



# Given a property and a dataset, get the value of the property.
prop_value <- function(x, data) UseMethod("prop_value")
#' @export
prop_value.prop_constant <- function(x, data) {
  rep(x$value, nrow(data))
}
#' @export
prop_value.prop_variable <- function(x, data) {
  # Calculate a "column"
  col <- eval(value(x$value), envir = data, enclos = x$env)

  if (!(length(col) == 1 || length(col) == nrow(data))) {
    stop("Length of calculated column '", prop_label(x), "' (", length(col),
      ") is not equal to 1 or the number of rows in data (", nrow(data), ").",
      call. = FALSE)
  }

  if (x$property == "key" && any(duplicated(col))) {
    warning("All values in column used for 'key' property should be unique,",
      " but some values are duplicated.")
  }

  rep(col, length.out = nrow(data))
}
#' @export
prop_value.prop_reactive <- prop_value.prop_variable


# The name of the property: used for naming the variable it produces in the
# vega data frame
prop_label <- function(x) UseMethod("prop_label")
#' @export
prop_label.prop_constant <- function(x) ""
#' @export
prop_label.prop_variable <- function(x) as_char(x$value)
#' @export
prop_label.prop_reactive <- function(x) as_char(reactive_id(x$value))

# Reports whether this is a scaled prop
prop_is_scaled <- function(prop) !is.null(prop$scale)

# Generate a vega object for the individual mark.
prop_vega <- function(x, default_scale) UseMethod("prop_vega")
#' @export
prop_vega.prop_constant <- function(x, default_scale) {
  compact(list(
    scale = if (prop_is_scaled(x)) x$scale,
    value = x$value,
    mult = x$mult,
    offset = x$offset
  ))
}
#' @export
prop_vega.prop_variable <- function(x, default_scale) {
  compact(list(
    scale = if (prop_is_scaled(x)) x$scale,
    field = paste0("data.", safe_vega_var(prop_label(x))),
    mult = x$mult,
    offset = x$offset
  ))
}
#' @export
prop_vega.prop_reactive <- prop_vega.prop_variable

#' Property domain.
#'
#' @param x property to dispatch on
#' @param data name of data set
prop_domain <- function(x, data) UseMethod("prop_domain")
#' @export
prop_domain.prop_constant <- function(x, data) {
  # FIXME: for scaled constants, this should really insert a literal value in
  #   to the domain, but it's not obvious how to do that in vega currently.
  NULL
}
#' @export
prop_domain.prop_variable <- function(x, data) {
  list(
    data = data,
    field = paste0("data.", safe_vega_var(prop_label(x)))
  )
}
#' @export
prop_domain.prop_reactive <- prop_domain.prop_variable


# Given a prop object, return a string representation of the value
# @examples
# p <- props(x = ~mpg, y = 10)
# as.character(p$x)
#
# p <- props(x := input_select(c("red", "blue")), y = 10)
# as.character.prop(p$x)
#' @export
as.character.prop_constant <- function(x, ...) as.character(x$value)
#' @export
as.character.prop_variable <- function(x, ...) deparse2(x$value)
#' @export
as.character.prop_reactive <- function(x, ...) reactive_id(x$value)

#' @export
format.prop <- function(x, ...) {
  if (!is.null(x$offset)) {
    offset <- paste0(" ", if (x$offset > 0) "+" else "-", " ", abs(x$offset))
  } else {
    offset <- ""
  }

  if (!is.null(x$mult)) {
    mult <- paste0(" * ", x$mult)
  } else {
    mult <- ""
  }
  scale <- if (prop_is_scaled(x)) x$scale else "<none>"
  event <- x$event %||% "<none>"
  type <- sub("^prop_", "", class(x)[1])

  paste0("<", type, "> ", as.character(x), offset, mult,
    " (property: ", x$property, ", scale: ", scale, ", event: ", event, ")")
}

#' @export
print.prop <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# Determine the variable type given a data frame and property.
#
# @param data The data object.
# @param prop The property object.
# @keywords internal
prop_type <- function(data, prop) {
  UseMethod("prop_type")
}
#' @export
prop_type.data.frame <- function(data, prop) {
  vector_type(prop_value(prop, data))
}

# Continuous variables are not countable; categorical variables are.
prop_countable <- function(data, prop) {
  countable_prop_type(prop_type(data, prop))
}

# Report whether a prop type is countable
countable_prop_type <- function(type) {
  switch(type,
    NULL = NULL,
    "numeric" = FALSE,
    "datetime" = FALSE,
    "ordinal" = TRUE,
    "nominal" = TRUE,
    "logical" = TRUE,
    stop("Don't know whether prop type '", type, "' is countable")
  )
}

#' @export
formula.prop <- function(x, ...) {
  eval(substitute(~x, list(x = x$value)), x$env)
}
