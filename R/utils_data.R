# Calculate the data_id for a data object; if the object already has the data_id
# attribute, simply return that.
get_data_id <- function(data, prefix = "unnamed_data") {
  if (!is.reactive(data)) stop("data object must be a reactive")

  # If there's already aa data_id attribute, just return it
  if (!is.null(attr(data, "data_id")))
    return(attr(data, "data_id"))

  paste0(prefix, "_", digest(data, algo = "crc32"))
}

# Add the appropriate data_id attribute to a data object, and return it
add_data_id <- function(data, prefix = "unnamed_data") {
  if (!is.reactive(data)) stop("data object must be a reactive")

  attr(data, "data_id") <- get_data_id(data, prefix)
  data
}


merge_df <- function(a, b) {
  if (is.null(a) || nrow(a) == 0 || ncol(a) == 0) return(b)
  if (is.null(b) || nrow(b) == 0 || ncol(b) == 0) return(a)

  cbind(a[setdiff(names(a), names(b))], b)
}

# Convenience function to remove missing values from a data.frame
# Remove all non-complete rows, with a warning if \code{warn_na = FALSE}.
#
# @param x An input data frame or split_df
# @param warn_na Warn when rows are removed?
# @param finite When FALSE, remove NA and NaN. When TRUE, also remove Inf and -Inf.
# @param vars Columns to check for missing values (for data.frame and split_df)
remove_missing <- function(x, warn_na = TRUE, finite = FALSE, ...) {
  UseMethod("remove_missing")
}

#' @export
remove_missing.default <- function(x, warn_na = TRUE, finite = FALSE) {
  if (!is.atomic(x)) {
    stop("Input to remove_missing.default must be a vector.")
  }

  if (finite) {
    missing <- !is.finite(x) | is.na(x)
    str <- "non-finite"
  } else {
    missing <- is.na(x)
    str <- "missing"
  }

  if (any(missing)) {
    x <- x[!missing]
    if (warn_na) warning("Removed ", sum(missing), " rows containing ", str,
      " values", ".", call. = FALSE)
  }
  x
}

#' @export
remove_missing.data.frame <- function(x, warn_na = TRUE, finite = FALSE,
    vars = names(x)) {

  vars <- intersect(vars, names(x))

  if (finite) {
    missing <- !finite.cases(x[, vars, drop = FALSE])
    str <- "non-finite"
  } else {
    missing <- !complete.cases(x[, vars, drop = FALSE])
    str <- "missing"
  }

  if (any(missing)) {
    x <- x[!missing, , drop = FALSE]
    if (warn_na) warning("Removed ", sum(missing), " rows containing ", str,
      " values", ".", call. = FALSE)
  }
  x
}

#' @export
remove_missing.split_df <- function(x, warn_na = TRUE, finite = FALSE,
    vars = names(x)) {
  x[] <- lapply(x, remove_missing, warn_na, vars, finite)
  x
}


# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
finite.cases <- function(x) {
  finite_cases <- vapply(x, is.finite, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(finite_cases)) {
    all(finite_cases)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(finite_cases)) == ncol(x)
  }
}


to_csv <- function(x, header = TRUE, ...) UseMethod("to_csv")

#' @export
to_csv.data.frame <- function(x, header = TRUE) {
  x <- lapply(x, format_vec_csv)

  # Collapse across rows, yielding each row of CSV text
  rows <- do.call(paste, c(x, sep = ","))
  rows <- paste0(rows, collapse = "\n")

  if (header) {
    rows <- paste(
      paste(quote_text(names(x)), collapse = ","),
      rows,
      sep = "\n"
    )
  }

  rows
}

#' @export
to_csv.split_df <- function(x, header = TRUE) {
  headers <- logical(length(x))
  # If we want a header, only add it for the first data frame in the split_df
  if (header) headers[1] <- TRUE

  paste(mapply(to_csv, x, header = headers), collapse = "\n")
}

# Format a vector for csv output
format_vec_csv <- function(vec) UseMethod("format_vec_csv")
#' @export
format_vec_csv.numeric <- function(vec) vec
#' @export
format_vec_csv.character <- function(vec) quote_text(vec)
#' @export
format_vec_csv.factor <- function(vec) quote_text(vec)
# Represent dates and times as numbers (ms from epoch). If they're represented
# as date-time strings, this will cause problems when the client is in a
# different time zone from the server.
#' @export
format_vec_csv.POSIXt <- function(vec) floor(as.numeric(vec) * 1000)
#' @export
format_vec_csv.Date <- function(vec) as.numeric(as.POSIXct(vec)) * 1000


# Format a vector for d3 json output
format_vec_d3json <- function(vec) UseMethod("format_vec_d3json")
#' @export
format_vec_d3json.numeric <- function(vec) vec
#' @export
format_vec_d3json.character <- function(vec) vec
#' @export
format_vec_d3json.factor <- function(vec) as.character(vec)
#' @export
format_vec_d3json.POSIXt <- function(vec) floor(as.numeric(vec) * 1000)
#' @export
format_vec_d3json.Date <- function(vec) as.numeric(as.POSIXct(vec)) * 1000


# Replace \ with \\, " with \", and add " to start and end
quote_text <- function(txt) {
  txt <- gsub("\\\\", "\\\\\\\\", txt, fixed = TRUE)
  txt <- gsub('"', '\\\\"', txt, fixed = TRUE)
  paste0('"', txt, '"')
}
