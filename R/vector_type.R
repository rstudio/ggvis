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

#' @S3method vector_type POSIXt
vector_type.POSIXt <- function(x) "datetime"
#' @S3method vector_type Date
vector_type.Date <- function(x) "datetime"
#' @S3method vector_type numeric
vector_type.numeric <- function(x) "numeric"
#' @S3method vector_type integer
vector_type.integer <- function(x) "numeric"
#' @S3method vector_type character
vector_type.character <- function(x) "nominal"
#' @S3method vector_type logical
vector_type.logical <- function(x) "logical"
#' @S3method vector_type factor
vector_type.factor <- function(x) "nominal"
#' @S3method vector_type ordered
vector_type.ordered <- function(x) "ordinal"
#' @S3method vector_type NULL
vector_type.NULL <- function(x) "NULL"
#' @S3method vector_type default
vector_type.default <- function(x) {
  stop("Unknown variable type: ", paste0(class(x), collapse = "/"))
}


#' Determine the vega data type for a vector
#'
#' This is used to specify the data type so that the appropriate parser is used
#' when Vega receives the data.
#' @param x A vector.
vega_data_parser <- function(x) UseMethod("vega_data_parser")

#' @S3method vega_data_parser POSIXt
vega_data_parser.POSIXt <- function(x) "date"
#' @S3method vega_data_parser Date
vega_data_parser.Date <- function(x) "date"
#' @S3method vega_data_parser numeric
vega_data_parser.numeric <- function(x) "number"
#' @S3method vega_data_parser integer
vega_data_parser.integer <- function(x) "number"
#' @S3method vega_data_parser character
vega_data_parser.character <- function(x) NULL
#' @S3method vega_data_parser logical
vega_data_parser.logical <- function(x) "boolean"
#' @S3method vega_data_parser factor
vega_data_parser.factor <- function(x) NULL
#' @S3method vega_data_parser ordered
vega_data_parser.ordered <- function(x) NULL
#' @S3method vega_data_parser NULL
vega_data_parser.NULL <- function(x) NULL
#' @S3method vega_data_parser default
vega_data_parser.default <- function(x) {
  stop("Unknown variable type: ", paste0(class(x), collapse = "/"))
}
