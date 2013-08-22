#' Create a property.
#' 
#' Properties are used to describe the visual properties of \link{marks}.
#' You create a single property defintion with \code{prop}, and manage
#' sets of named properties with \code{\link{props}} (which also provides
#' shortcuts for creating the most common kind of properties)
#'
#' @param x The value of the property. This can be an atomic vector 
#'   (a constant), a name or quoted call (a variable), or a delayed
#'   reactive (which can be either variable or constant).
#' @param scale If \code{TRUE} uses the default scale associated with property;
#'   If \code{FALSE}, does not scale the value. Otherwise supply a string to
#'   select a custom scale. If \code{x} is an interactive input, then this
#'   defaults to the scale parameter of the input.
#' @param offset,mult Additive and multiplicate pixel offset used to adjust 
#'   scaled values. These are useful if you want to place labels offset from
#'   points.
#' @param env If \code{x} is a quoted call this provides the environment in 
#'   which to look for variables not in the data. You should not need this in
#'   ordinary operation. 
#' @seealso \code{\link{props}} to manage multiple properties and to 
#'   succintly create the most common types.
#' @export
#' @examples
#' prop(1)
#' prop(quote(cyl))
#' 
#' # If you have a variable name as a string
#' var <- "cyl"
#' prop(as.name(var))
#' 
#' # Override regular scale
#' prop(quote(cyl), scale = "y-2")
#' 
#' # Don't scale variable (i.e. it already makes sense in the visual space)
#' prop(quote(colour), scale = FALSE)
prop <- function(x, scale = NULL, offset = NULL, mult = NULL,
                 env = parent.frame()) {

  if (is.atomic(x)) {
    type <- "constant"
    assert_that(length(x) == 1)
    # Constants don't need to capture environment
    env <- NULL
    dr <- NULL
    scale <- scale %||% FALSE

  } else if (is.input(x)) {
    type <- "reactive"
    dr <- x
    x <- function() stop("Delayed reactive has not yet been advanced!")
    scale <- scale %||% dr$scale %||% FALSE

  } else if (is.quoted(x)) {
    type <- "variable"
    dr <- NULL
    scale <- scale %||% TRUE

  } else {
    stop("Unknown input to prop", call = FALSE)
  }

  structure(
    list(value = x,
      dr = dr,
      type = type,
      scale = scale,
      offset = offset,
      mult = mult,
      env = env
    ),
    class = c("prop")
  )
}

#' @export
#' @rdname prop
is.prop <- function(x) inherits(x, "prop")

# Given a property and a dataset, get the value of the property.
prop_value <- function(x, data, processed = FALSE) {
  if (processed) return(data[[prop_name(x)]])
  if (x$type == "constant") return(rep(x$value, nrow(data)))

  # Get the expression to evaluate
  if (x$type == "reactive") {
    expr <- x$value()
  } else {
    expr <- x$value
  }

  # Calculate a "column"
  col <- eval(expr, envir = data, enclos = x$env)

  if (!(length(col) == 1 || length(col) == nrow(data))) {
    stop("Length of calculated column '", prop_name(x), "' (", length(col),
      ") is not equal to 1 or the number of rows in data (", nrow(data), ").",
      call. = FALSE)
  }

  rep_len(col, nrow(data))
}

# The name of the property: used for naming the variable it produces in the
# vega data frame
prop_name <- function(x) {
  switch(x$type,
    constant = "",
    reactive = x$dr$id, 
    variable = safe_vega_var(x$value))
}

# The scale (if any) that this property needs
prop_scale <- function(x, default_scale) {
  if (isTRUE(x$scale)) {
    default_scale
  } else if (is.character(x$scale)) {
    x$scale
  } else {
    NA
  }
}

# Generate a vega object for the individual mark.
prop_vega <- function(x, default_scale) {
  scale <- prop_scale(x, default_scale)
  pv <- list(
    scale = if (!is.na(scale)) scale,
    mult = x$mult,
    offset = x$offset
  )

  if (x$type == "constant") {
    pv$value <- x$value
  } else {
    pv$field <- paste0("data.", prop_name(x))
  }

  compact(pv)
}

#' Property domain.
#'
#' @param x property to dispatch on
#' @param data name of data set
prop_domain <- function(x, data) {
  # FIXME: for scaled constants, this should really insert a literal value in
  #   to the domain, but it's not obvious how to do that in vega currently.
  if (x$type == "constant") return(NULL)

  list(
    data = data,
    field = paste0("data.", prop_name(x))
  )
}


# Given a prop object, return a string representation of the value
# @examples
# p <- props(x ~ mpg, y = 10)
# as.character(p$x)
#
# p <- props(x = input_select(c("red", "blue")), y = 10)
# as.character.prop(p$x)
#' @S3method as.character prop
as.character.prop <- function(x, ...) {
  switch(x$type,
    constant = as.character(x$value),
    reactive = x$dr$id,
    variable = deparse(x$value)
  )
}

#' @S3method format prop
format.prop <- function(x, ...) {
  if (identical(x$scale, TRUE)) {
    scale <- "auto"
  } else if (identical(x$scale, FALSE)) {
    scale <- "none"
  } else {
    scale <- x$scale
  }
  
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
  
  paste0("<", x$type, "> ", as.character(x), offset, mult, 
    " (scale: ", scale, ")")
}

#' @S3method print prop
print.prop <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# Determine the variable type given a data frame and property.
#
# @param data The data object.
# @param prop The property object.
# @param processed Has this data object been processed so that new columns
#   have been calculated and unused columns have been dropped?
# @keywords internal
prop_type <- function(data, prop, processed = FALSE) {
  UseMethod("prop_type")
}

#' @S3method prop_type split_df
prop_type.split_df <- function(data, prop, processed = FALSE) {
  types <- vapply(data, prop_type, prop = prop, processed = processed,
    FUN.VALUE = character(1))
  if (!all_same(types)) {
    stop("Inconsistent types", call. = FALSE)
  }
  types[1]
}
#' @S3method prop_type data.frame
prop_type.data.frame <- function(data, prop, processed = FALSE) {
  prop_type(prop_value(prop, data, processed))
}
#' @S3method prop_type POSIXt
prop_type.POSIXt <- function(data, prop) "datetime"
#' @S3method prop_type Date
prop_type.Date <- function(data, prop) "datetime"
#' @S3method prop_type numeric
prop_type.numeric <- function(data, prop) "numeric"
#' @S3method prop_type integer
prop_type.integer <- function(data, prop) "numeric"
#' @S3method prop_type character
prop_type.character <- function(data, prop) "nominal"
#' @S3method prop_type logical
prop_type.logical <- function(data, prop) "logical"
#' @S3method prop_type factor
prop_type.factor <- function(data, prop) "nominal"
#' @S3method prop_type ordered
prop_type.ordered <- function(data, prop) "ordinal"
#' @S3method prop_type NULL
prop_type.NULL <- function(data, prop) "NULL"
#' @S3method prop_type default
prop_type.default <- function(data, prop) {
  stop("Unknown variable type: ", paste0(class(data), collapse = "/"))
}

#' Determine the numeric range of a variable
#'
#' @keywords internal
prop_range <- function(data, prop, na.rm = TRUE) {
  UseMethod("prop_range")
}
#' @S3method prop_range data.frame
prop_range.data.frame <- function(data, prop, na.rm = TRUE) {
  range(prop_value(prop, data), na.rm = na.rm)
}
#' @S3method prop_range split_df
prop_range.split_df <- function(data, var, na.rm = TRUE) {
  ranges <- vapply(data, prop_range, var, na.rm = na.rm,
    FUN.VALUE = numeric(2))
  range(ranges)
}
