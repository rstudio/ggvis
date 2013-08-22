
last <- function(x) x[length(x)]

dots <- function(...) {
  eval(substitute(alist(...)))
}

dot_names <- function(...) {
  args <- dots(...)
  
  nms <- names(args) %||% rep("", length(args))
  missing <- nms == ""
  if (all(!missing)) return(args)
  
  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1), USE.NAMES = FALSE)
  
  nms[missing] <- defaults
  nms
}


"%||%" <- function(a, b) if (!is.null(a)) a else b

# Given a vector or list, drop all the NULL items in it
drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

# Given two named vectors, join them together, and keep only the last element
# with a given name in the resulting vector. If b has any elements with the
# same name as elements in a, the element in a is dropped. Also, if there are
# any duplicated names in a or b, only the last one with that name is kept.
merge_vectors <- function(a, b) {
  if ((!is.null(a) && length(a) > 1 && is.null(names(a))) ||
      (!is.null(b) && length(b) > 1 && is.null(names(b)))) {
    stop("merge_vectors: vectors must be either NULL or named vectors")
  }

  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}

merge_df <- function(a, b) {
  if (is.null(a) || nrow(a) == 0 || ncol(a) == 0) return(b)
  if (is.null(b) || nrow(b) == 0 || ncol(b) == 0) return(a)

  cbind(a[setdiff(names(a), names(b))], b)
}

# Tests whether all elements in a vector are the same, respecting NA.
# Returns TRUE for zero-length vectors
all_same <- function(x) {
  nas <- is.na(x)
  if (length(x) == 0 || all(nas))
    TRUE
  else if (any(nas))
    FALSE
  else
    all(x == x[1])
}

# Test whether a file exists and is a directory
dir.exists <- function(x) {
  res <- file.exists(x) & file.info(x)$isdir
  setNames(res, x)
}

# Check whether a package is installed, and stop if not
assert_installed <- function(pkg) {
  if (nchar(system.file(package = pkg)) == 0) {
    stop("The '", pkg, "' package is required for this functionality")
  }
}

# Determine if an object is the result of quote()
is.quoted <- function(x) {
  is.call(x) || is.name(x)
}

# Drop all empty items from a list - except environments, which stay even if
# they are empty.
compact <- function(x) {
  non_empty <- !vapply(x, empty, logical(1))
  env <- vapply(x, is.environment, logical(1))
  x[non_empty | env]
}

param_string <- function(x, collapse = TRUE) {
  is_reactive <- vapply(x, is.reactive, logical(1))
  is_env <- vapply(x, is.environment, logical(1))
  is_string <- vapply(x, is.character, logical(1))

  x[is_reactive] <- "<reactive>"
  x[is_env] <- vapply(x[is_env], format, character(1))
  values <- vapply(x, toString, character(1))
  values[is_string] <- paste0("'", encodeString(values[is_string]), "'")

  if (!collapse) return(values)
  paste0("(", paste0(names(x), " = ", values, collapse = ", "), ")")
}

# Given a string, return a string that is safe as a vega variable.
# Replaces . with \.
safe_vega_var <- function(x) {
  if (is.name(x)) {
    x <- as.character(x)
  } else if (is.quoted(x)) {
    x <- paste0(deparse(x, width = 500), collapse = "")
  }
  
  gsub(".", "\\.", x, fixed = TRUE)
}

empty <- function(x) length(x) == 0

quickdf <- function(list) {
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -length(list[[1]]))

  list
}

# Generate a random number to use in IDs
rand_id <- function(prefix = "") {
  paste0(prefix, floor(runif(1, 1e8, 1e9-1)))
}

is_missing <- function(x) identical(x, quote(expr = ))

# Check if the calling function had missing arguments when it was called, and
# throw an informative error if so. This happens when there are extra commas in
# the call, as in f(a, 2, ).
check_empty_args <- function() {
  call <- sys.call(-1)
  args <- as.list(call[-1])

  missing <- vapply(args, is_missing, logical(1))

  if (!any(missing)) return(invisible(TRUE))

  stop("Extra comma at position", if (sum(missing) > 1) "s", " ",
    paste0(which(missing), collapse = ", "),
    " in call to ", as.character(call[[1]]), "()",
    call. = FALSE)
}
