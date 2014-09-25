#' Get data from a ggvis object
#'
#' This function is useful for inspecting the data in a ggvis object.
#' @param vis A ggvis object.
#' @examples
#' p <- cocaine %>% ggvis(~price) %>% layer_bars()
#' get_data(p)
#'
#' @export
get_data <- function(vis) {
  if (!is.ggvis(vis)) stop("vis must be a ggvis object.")
  lapply(vis$data, function(x) shiny::isolate(x()))
}

data_id <- function(x) {
  return(attr(x, "data_id", TRUE))
}

`data_id<-` <- function(x, value) {
  attr(x, "data_id") <- value
  x
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


# Replace \. with . , " with \", and add " to start and end
quote_text <- function(txt) {
  if (length(txt) == 0)
    return(txt)

  txt <- gsub("\\.", ".", txt, fixed = TRUE)
  txt <- gsub('"', '\\\\"', txt, fixed = TRUE)
  paste0('"', txt, '"')
}


cur_data <- function(x) shiny::isolate(x$cur_data())
cur_props <- function(x) x$cur_props

eval_vector <- function(x, f) UseMethod("eval_vector")
eval_vector.data.frame <- function(x, f) {
  eval(f[[2]], x, environment(f))
}

# Find the range of values for a vector
data_range <- function(x) UseMethod("data_range")
#' @export
data_range.default <- function(x) range2(x, na.rm = TRUE)
#' @export
data_range.character <- function(x) unique(na.omit(x))
#' @export
data_range.factor <- function(x) levels(x)

# Takes a list of vectors, and puts them all together into one vector.
# For POSIXct, this preserves time zone.
# For factors, this preserves all levels (but not necessarily order)
concat <- function(x) {
  x <- drop_nulls(x)
  if (length(x) == 0) {
    return(NULL)
  }
  if (inherits(x[[1]], "POSIXct")) {
    vec <- do_call(c, .args = x)
    structure(vec, tzone = attr(x[[1]], "tzone"))
  } else if (inherits(x[[1]], "Date")) {
    structure(unlist(x, recursive = FALSE), class = "Date")
  } else {
    unlist(x, recursive = FALSE)
  }
}

# Does the same as base::range, except that for for 0-length vectors, it returns
# a zero-length vector of appropriate type, instead of throwing an error.
range2 <- function(..., na.rm = FALSE) {
  vals <- c(...)
  if (length(vals) == 0) {
    return(vals)
  }
  range(..., na.rm = na.rm)
}
