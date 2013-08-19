#' S3 property class
#'
#' @keywords internal
#' @export
prop <- function(x, constant = NULL, scale = NULL,
                 offset = NULL, mult = NULL) {

  # TODO: detect constant/variable for reactives
  # Atomic values and reactives default to constant = TRUE
  constant <- constant %||% (is.atomic(x) || is.reactive(x))

  # Constant scales default to FALSE; variable scales default to TRUE
  scale <- scale %||% !constant

  reactive <- is.delayed_reactive(x)
  dr <- NULL
  if (reactive) {
    dr <- x
    x <- function() stop("Delayed reactive has not yet been advanced!")
  }

  structure(
    list(value = x,
      dr = dr,
      constant = constant,
      scale = scale,
      reactive = reactive,
      offset = offset,
      mult = mult
    ),
    class = c("prop")
  )
}

#' @export
#' @rdname prop
is.prop <- function(x) inherits(x, "prop")

as.prop <- function(x) UseMethod("as.prop")
#' @S3method as.prop character
as.prop.character <- function(x) prop_var(as.name(x))
#' @S3method as.prop name
as.prop.name <- function(x) prop_var(x)
#' @S3method as.prop call
as.prop.call <- function(x) prop_var(x)
#' @S3method as.prop prop
as.prop.prop <- function(x) x

# Given a property and a dataset, get the value of the property.
prop_value <- function(x, data, processed = FALSE) {
  if (x$reactive) {
    val <- x$value()
  } else {
    val <- x$value
  }

  if (x$constant) {
    rep(val, nrow(data))

  } else if (x$reactive) {
    data[[val]]

  } else {
    if (processed) {
      data[[prop_name(x)]]
    } else {
      eval(val, data, baseenv())
    }
  }
}

# The name of the property: used for naming the variable it produces in the
# vega data frame
prop_name <- function(x) {
  if (x$reactive) return(x$dr$id)
  if (x$constant) return("")

  # If we got here, it's a non-reactive variable
  var <- x$value

  if (is.symbol(var)) {
    as.character(var)

  } else if (is.language(var)) {
    # var is calculated from an expression; get a unique, JS-safe name. Prepend
    # a string to so that an expression with same text as a var will have a
    # different hash, e.g., the expression wt/mpg vs. the variable `wt/mpg`.
    safe_vega_var(paste0("[e]", deparse(var)))
    
  } else {
    stop("Unknown type for var", call. = FALSE)
  }
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

  if (x$constant && !x$reactive) {
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
  if (x$constant && !x$reactive) return(NULL)

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
# p <- props(x = prop(input_select(c("red", "blue")), TRUE, FALSE), y = 10)
# as.character.prop(p$x)
#' @S3method as.character prop
as.character.prop <- function(x, ...) {
  if (x$reactive) {
    x$dr$id
  } else if (x$constant) {
    as.character(x$value)
  } else {
    deparse(x$value)
  }
}

#' @S3method format prop
format.prop <- function(x, ...) {
  if (x$reactive) {
    paste0("<reactive> ", x$dr$id)

  } else if (x$constant) {
    if (identical(x$scale, TRUE)) {
      scale <- " [auto]"
    } else if (is.character(x$scale)) {
      scale <- paste0(" [", x$scale, "]")
    } else {
      scale <- ""
    }
    params <- param_string(compact(x[c("mult", "offset")]), collapse = FALSE)

    paste0("<const> ", x$value, scale, "\n",
      if (length(params) > 0) paste0(" * ", names(params), " ", params, collapse = "\n")
    )

  } else {
    paste0("<variable> ", deparse(x$value))
  }

}

#' @S3method print prop
print.prop <- function(x, ...) cat(format(x, ...), "\n", sep = "")



is_constant <- function(x) {
  if (!is.prop(x)) stop("x must be a prop object.", call. = FALSE)
  x$constant
}
is_variable <- function(x) !is_constant(x)



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
