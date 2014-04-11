# Given an expression, this returns a function with . inserted.
defer <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)

  args <- pairlist(. = bquote())
  body <- insert_arg(expr)
  eval(call("function", args, body), env)
}

# Given an expression, this returns an expression with sym inserted as the first
# argument to the first function call. There is special handling if the function
# is an operator.
#
# Examples:
# a()           ==>  a(.)
# a() + b()     ==>  a(.) + b()
# a             ==>  Error (first item must be a function call)
# a + b()       ==>  Error (first item must be a function call)
# a() %>% b()   ==>  a(.) %>% b()
# a(5) %>% b()  ==>  a(., 5) %>% b()
# b(a())        ==>  b(., a())
insert_arg <- function(expr, sym = ".") {
  # If it's a bare symbol, like `f` (and not `f()`), just return it.
  if(is.symbol(expr)) {
    stop("Can't insert symbol as first argument because expr is not a function.")
  }

  # If we've gotten here, it's a function.
  # Special case for operators: recurse into tree
  if (is.operator(expr[[1]])) {
    el <- as.list(expr)
    el[[2]] <- insert_arg(el[[2]])
    return(as.call(el))
  }

  # If we've gotten here, it's a function where we want to insert the 'sym'
  # as the first argument
  el <- as.list(expr)
  el <- c(el[1], as.symbol(sym), el[-1])
  as.call(el)
}

is.operator <- function(x) {
  if (!is.symbol(x)) stop("x must be a symbol.")

  x <- as.character(x)

  grepl('^%.*%$', x) | x %in% c('::', ':::', '$', '@', '[', '[[', ':',
                                '^', '*', '/', '+', '-',
                                '>', '>=', '<', '<=', '==', '!=',
                                '!', '&', '&&', '|', '||',
                                '~', '<-', '<<-', '=')
}
