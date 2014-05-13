
# Callback helper functions ---------------------------------------------------

# Check that input is a funtion with the specified arguments
check_callback <- function(f, args) {
  if (!is.null(f)) return()
  fname <- deparse2(substitute(f))

  if (!is.function(f)) {
    stop(fname, " is not a function", call. = FALSE)
  }

  f_args <- names(formals(f))
  if (any(f_args == "...")) return()

  if (!identical(f_args, args)) {
    stop(fname, " needs arguments: ", paste(args, collapse = ", "),
      call. = FALSE)
  }
}

# Given callback, id and session, setup an observer
setup_callback <- function(f, id, session) {
  if (is.null(f)) return()

  shiny::observe({
    value <- session$input[[id]]
    f(value, session)
  })
}
