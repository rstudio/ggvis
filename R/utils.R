
args_as_list <- function() {
  call <- sys.call(-1)
  std <- match.call(eval(call[[1]]), call = call)
  args <- as.list(std[-1])

  lapply(args, eval, parent.frame())
}

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
