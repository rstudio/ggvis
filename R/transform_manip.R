#' Subset, summarise and mutate your data
#' 
#' @examples
#' # Dynamically restrict range using transform subset
#' ggvis(mtcars, 
#'    transform_subset(cyl > eval(input_slider(0, 10))), 
#'    props(x = ~disp, y = ~mpg), 
#'    mark_symbol())
#' 
#' # Dynamically compute box-cox transformation with transform_mutate
#' bc <- function(x, lambda) {
#'   if (abs(lambda) < 1e-6) log(x) else (x ^ lambda - 1) / lambda
#' }
#' bc_slider <- input_slider(-2, 2, 1, step = 0.1)
#' ggvis(mtcars, 
#'  transform_mutate(disp = bc(disp, eval(bc_slider))), 
#'  props(x = ~disp, y = ~mpg), 
#'  mark_symbol())
#' 
#' @name transform_manip
NULL

#' @export
#' @rdname transform_manip
transform_subset <- function(..., .env = parent.frame()) {
  expr <- named_dots(...)
  
  out <- extract_inputs(expr)
  transform(c("subset", "manip"), 
    expr = out$expr, 
    inputs = out$inputs, 
    env = .env) 
}

#' @export
#' @rdname transform_manip
transform_summarise <- function(..., .env = parent.frame()) {
  expr <- named_dots(...)
  
  out <- extract_inputs(expr)
  transform(c("summarise", "manip"), 
    expr = out$expr, 
    inputs = out$inputs, 
    env = .env) 
}

#' @export
#' @rdname transform_manip
transform_mutate <- function(..., .env = parent.frame()) {
  expr <- named_dots(...)
  
  out <- extract_inputs(expr)
  transform(c("mutate", "manip"), 
    expr = out$expr, 
    inputs = out$inputs, 
    env = .env) 
}

# transform_manip methods ------------------------------------------------------

#' @S3method connect transform_manip
connect.transform_manip <- function(x, props, source = NULL, session = NULL) {  
  inputs <- lapply(x$inputs, as.reactive, session = session)
  
  reactive({
    if (is.function(source)) source <- source()

    values <- lapply(inputs, function(f) f())
    env <- list2env(values, parent = x$env)
    
    manip(x, source, x$expr, env)
  })
}

#' @S3method is.dynamic transform_manip
is.dynamic.transform_manip <- function(x) {
  length(x$inputs) > 0 
}

#' @S3method controls transform_manip
controls.transform_manip <- function(x) {
  controls.list(x$inputs)
}

# individual implementations ---------------------------------------------------

manip <- function(x, data, expr, env) UseMethod("manip")

#' @S3method manip transform_subset
manip.transform_subset <- function(x, data, expr, env) {
  r <- vapply(conds, eval, env = data, enclos = env,
    FUN.VALUE = logical(nrow(data)))
  
  all <- rowSums(r, na.rm = TRUE) == ncol(r)
  data[all, , drop = FALSE]
}

#' @S3method manip transform_mutate
manip.transform_mutate <- function(x, data, expr, env) {
  data_env <- list2env(data, parent = env)
  
  for(i in seq_along(expr)) {
    data_env[[names(expr)[i]]] <- eval(expr[[i]], data_env)
  }
  
  out_cols <- union(names(data), names(expr))
  as_df(mget(out_cols, data_env))
}

#' @S3method manip transform_summarise
manip.transform_summarise <- function(x, data, expr, env) {
  data_env <- list2env(data, parent = env)
  
  for (i in seq_along(expr)) {
    data_env[[names(expr)[i]]] <- eval(expr[[i]], data_env)
  }
  
  as_df(mget(unique(names(expr)), data_env))
}

as_df <- function(x, n = length(.subset2(x, 1))) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}

# extract interactive inputs from expr -----------------------------------------

extract_inputs <- function(x, env = parent.frame()) {
  # Base case
  if (is.name(x) || is.atomic(x)) {
    return(list(expr = x, inputs = NULL))
  }
  
  # If it's a call to eval, it's an input and should be evaluated
  if (is.call(x) && identical(x[[1]], quote(eval))) {
    stopifnot(length(x) == 2)
    input <- eval(x[[2]], env)
    stopifnot(is.input(input))
    
    nm <- input$id
    
    return(list(
      expr = as.name(nm),
      inputs = setNames(list(input), nm) 
    ))
  }
  
  # Otherwise it must be another call, or a list, in which case recurse
  if (is.list(x)) {
    args_out <- lapply(x, extract_inputs, env = env)
  } else if (is.call(x)) {
    args_out <- lapply(x[-1], extract_inputs, env = env)    
  } else {
    stop("Unknown input type: ", paste0(class(x), collapse = "/"), 
      call. = FALSE)
  }
  
  expr <- lapply(args_out, "[[", "expr")
  if (is.call(x)) expr <- as.call(c(x[[1]], expr))
  
  inputs <- unlist(lapply(unname(args_out), "[[", "inputs"), recursive = FALSE)
  inputs <- inputs[!duplicated(names(inputs))]
  
  list(
    expr = expr,
    inputs = inputs
  )   
}
