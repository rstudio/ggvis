last <- function(x) {
  if (length(x) == 0) return(NULL)
  x[[length(x)]]
}

make_call <- function(f, ..., .args = list()) {
  if (is.character(f)) f <- as.name(f)
  as.call(c(list(f, ...), .args))
}
do_call <- function(f, ..., .args = list(), .env = parent.frame(), .debug = FALSE) {
  f <- substitute(f)

  call <- make_call(f, ..., .args = .args)
  if (.debug) print(call)
  eval(call, .env)
}


deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")

names2 <- function(x) names(x) %||% rep("", length(x))
named <- function(x) names2(x) != ""

"%||%" <- function(a, b) if (!is.null(a)) a else b

# Given a vector or list, drop all the NULL items in it
drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
    paste0("\\1", paste(rep(" ", indent), collapse = "")),
    str,
    perl = TRUE
  )
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

# Tests whether all elements in a vector are the same, respecting NA.
# Returns TRUE for zero-length vectors
# Returns FALSE for non-atomic vectors
all_same <- function(x) {
  if (!is.atomic(x)) {
    return(FALSE)
  }

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
  is_reactive <- vapply(x, shiny::is.reactive, logical(1))
  is_env <- vapply(x, is.environment, logical(1))
  is_string <- vapply(x, is.character, logical(1))

  x[is_reactive] <- "<reactive>"
  x[is_env] <- vapply(x[is_env], format, character(1))
  values <- vapply(x, toString, character(1))
  values[is_string] <- paste0("'", encodeString(values[is_string]), "'")

  if (!collapse) return(values)
  paste0("(", paste0(names(x), " = ", values, collapse = ", "), ")")
}

# Convert various objects to char strings.
as_char <- function(x) UseMethod("as_char")
#' @export
as_char.name <- function(x) as.character(x)
#' @export
as_char.call <- function(x) deparse2(x)
#' @export
as_char.default <- function(x) as.character(x)

# Given a string, return a string that is safe as a vega variable.
# Replaces . with \.
safe_vega_var <- function(x) {
  gsub(".", "\\.", x, fixed = TRUE)
}

empty <- function(x) UseMethod("empty")

#' @export
empty.default <- function(x) length(x) == 0

# Convert a list to a data frame, quickly
quickdf <- function(list) {
  if (length(list) == 0) return(data.frame())

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

# Report whether a package is installed
is_installed <- function(pkg) {
  system.file(package = pkg) != ""
}

# Try to load a package
try_require <- function(pkg) {
  result <- suppressPackageStartupMessages(suppressWarnings(
    require(pkg, character.only = TRUE)))

  if (!result) {
    stop(pkg,
      " package required for this functionality.  Please install and try again.",
      call. = FALSE)
  }
}


notify_guess <- function(x, explanation = NULL) {
  msg <- paste0(
    "Guessing ", deparse(substitute(x)), " = ", format(x, digits = 3),
    if (!is.null(explanation)) paste0(" # ", explanation)
  )
  message(msg)
}


any_apply <- function(xs, f) {
  for (x in xs) {
    if (f(x)) return(TRUE)
  }
  FALSE
}

pluck <- function(x, name) {
  lapply(x, `[[`, name)
}

vpluck <- function(x, name, type) {
  vapply(x, `[[`, name, FUN.VALUE = type)
}

# Like as.numeric, except that as.numeric(NULL) returns numeric(0), whereas
# as_numeric(NULL) returns NULL.
as_numeric <- function(x) {
  if (is.null(x)) NULL
  else as.numeric(x)
}

deprecated <- function(old, new = NULL, msg = NULL, version = NULL) {
  text <- paste0(
    sprintf("'%s' is deprecated.", old),
    if (!is.null(new)) sprintf(" Please use '%s' instead.", new),
    msg,
    if (!is.null(version)) sprintf(" (Last used in version %s)", version)
  )
  warning(text, call. = FALSE)
}

# Need this so R CMD check doesn't complain about "no visible global function
# definition"
`:=` <- function(x, value) {
  stop("This code should not be reached.", call. = FALSE)
}
