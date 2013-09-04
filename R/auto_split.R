#' @export
auto_split <- function() {
  pipe(c("auto_split", "split"))
}

#' @S3method format auto_split
format.auto_split <- function(x, ...) {
  " -> auto_split"
}

#' @S3method connect auto_split
connect.auto_split <- function(x, props, source = NULL, session = NULL) {
  source <- as.reactive(source)
  reactive({
    split_vars <- vapply(props, prop_countable, data = source(),
      FUN.VALUE = logical(1))

    # Get quoted expressions to split on
    split_vars <- unname(lapply(props[split_vars], function(prop) prop$value))

    split_df(source(), split_vars, env = x$env)
  })
}

#' @S3method empty auto_split
empty.auto_split <- function(x) FALSE
