# Given 'data', find the relevant data object. 'data' can be a string, function,
# or data object.
find_data_object <- function(data, envir = NULL) {
  if (is.function(data)) {
    # If it's a function, run it.
    # TODO: Inject `input` and `session` into data function
    data <- data()
    if (!is.data.frame(data) && !is.null(data)) {
      stop("Unexpected function result")
    }
  } else if (is.character(data)) {
    # If it's a string, get it from the envir. (Same as view_static)
    data <- get(data, envir = envir)
  } else if (is.data.frame(data)) {
    # If it's a data frame, just use it.
  } else if (is.null(data)) {
    # If it's null, do nothing
  } else {
    stop("Unexpected expression")
  }

  data
}
