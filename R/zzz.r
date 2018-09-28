.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()
  packageStartupMessage("The ggvis API is currently rapidly evolving. ",
    "We strongly recommend that you do not rely on this for production, but ",
    "feel free to explore. If you encounter a clear bug, please file a ",
    "minimal reproducible example at https://github.com/rstudio/ggvis/issues. ",
    "For questions and other discussion, please use ",
    "https://groups.google.com/group/ggvis.")
}

.onLoad <- function(libname, pkgname) {
  # ggvis provides methods for knitr::knit_print, but knitr isn't a Depends or
  # Imports of ggvis, only a Suggests. This code snippet manually registers
  # our method(s) with S3 once both ggvis and knitr are loaded.
  register_s3_method("knitr", "knit_print", "ggvis")
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
