#' Determine the "type" of a vector
#'
#' The \code{vector_type} collapses down the class of base vectors into
#' something useful more for visualisation, yielding one of "datetime",
#' "numeric", "ordinal", "nominal" or "logical".
#'
#' @param x a vector
#' @export
#' @seealso \code{default_scale}, which uses this when picking the default
#'   scale.
vector_type <- function(x) UseMethod("vector_type")

#' @export
vector_type.POSIXt <- function(x) "datetime"
#' @export
vector_type.Date <- function(x) "datetime"
#' @export
vector_type.numeric <- function(x) "numeric"
#' @export
vector_type.integer <- function(x) "numeric"
#' @export
vector_type.character <- function(x) "nominal"
#' @export
vector_type.logical <- function(x) "logical"
#' @export
vector_type.factor <- function(x) "nominal"
#' @export
vector_type.ordered <- function(x) "ordinal"
#' @export
vector_type.NULL <- function(x) "NULL"
#' @export
vector_type.default <- function(x) {
  stop("Unknown variable type: ", paste0(class(x), collapse = "/"))
}

# Reports whether a vector maps to a countable prop type
vector_countable <- function(x) {
  countable_prop_type(vector_type(x))
}

#' Determine the vega data type for a vector
#'
#' This is used to specify the data type so that the appropriate parser is used
#' when Vega receives the data.
#' @param x A vector.
vega_data_parser <- function(x) UseMethod("vega_data_parser")

#' @export
vega_data_parser.POSIXt <- function(x) "number"
#' @export
vega_data_parser.Date <- function(x) "number"
#' @export
vega_data_parser.numeric <- function(x) "number"
#' @export
vega_data_parser.integer <- function(x) "number"
#' @export
vega_data_parser.character <- function(x) NULL
#' @export
vega_data_parser.logical <- function(x) "boolean"
#' @export
vega_data_parser.factor <- function(x) NULL
#' @export
vega_data_parser.ordered <- function(x) NULL
#' @export
vega_data_parser.NULL <- function(x) NULL
#' @export
vega_data_parser.default <- function(x) {
  stop("Unknown variable type: ", paste0(class(x), collapse = "/"))
}
