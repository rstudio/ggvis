dots <- function(...) {
  eval(substitute(alist(...)))
}

standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)

  match.call(f, call)
}

modify_call <- function(call, new_args) {
  call <- standardise_call(call)
  nms <- names(new_args) %||% rep("", length(new_args))

  if (any(nms == "")) {
    stop("All new arguments must be named", call. = FALSE)
  }

  for(nm in nms) {
    call[[nm]] <- new_args[[nm]]
  }
  call
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
  if ((!is.null(a) && is.null(names(a))) ||
      (!is.null(b) && is.null(names(b)))) {
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

# Gives unique names to an unnamed set of items. The names are automatically
# generated and are designed to be "universally" unique (though current
# implementation falls far short!).
# TODO: if data is duplicated, don't add another symbol for it
SymbolTable <- setRefClass(
  'SymbolTable',
  fields = list(
    .symbols = 'environment',
    .default_prefix = 'character'
  ),
  methods = list(
    initialize = function(default_prefix = "item") {
      .symbols <<- new.env()
      .default_prefix <<- default_prefix
    },
    # Add a source; return the unique ID generated for the source
    add_item = function(item, prefix = .default_prefix) {
      id <- .unique_id(prefix)
      .symbols[[id]] <<- item
      return(id)
    },
    # Reports whether a given id is in the symbol table.
    contains = function(id) {
      # If not a string, return FALSE
      if (!is.character(id))
        return(FALSE)

      return(exists(id, .symbols, inherits = FALSE))
    },
    get = function(id) {
      return(.symbols[[id]])
    },
    to_list = function() {
      as.list(.symbols)
    },
    # Generate a unique ID, starting with the given prefix. The prefix should be
    # a valid JavaScript identifier (must start with a letter or underscore, can
    # contain letters, numbers, or underscore).
    .unique_id = function(prefix) {
      # TODO: Use better unique ID
      sprintf('%s_%d', prefix, as.integer(runif(1, min=1e8, max=1e9-1)))
    }
  )
)

# Determine if an object is the result of quote()
is.quoted <- function(x) {
  is.atomic(x) || is.call(x) || is.name(x)
}

compact <- function(x) Filter(Negate(is.null), x)

param_string <- function(x) {
  is_string <- vapply(x, is.character, logical(1))
  values <- vapply(x, toString, character(1))
  values[is_string] <- paste0("'", encodeString(values[is_string]), "'")

  paste0("(", paste0(names(x), " = ", values, collapse = ", "), ")")
}

# Given a string, return a string that is safe as a Javascript variable. The
# returned string consists of the first 10 "JS-safe" characters (for
# readability), and if hash=TRUE, a hash of the entire string is appended (for
# uniqueness).
safe_jsvar <- function(x) {
  paste(substr(gsub("[^a-zA-Z0-9]", "", x), 1, 10),
        digest(x, algo = "crc32"), sep = "_")
}

# Returns TRUE if the string is a safe Javascript variable name, false otherwise
is_safe_jsvar <- function(x) !grepl("[^a-zA-Z0-9]", x)
