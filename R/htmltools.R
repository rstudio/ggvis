# Will eventually move back into HTMLtools

html_dependency <- function(name, version, path, meta = NULL, script = NULL,
                            stylesheet = NULL, head = NULL) {
  structure(class = "html_dependency", list(
    name = name,
    version = as.character(version),
    path = path,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

#' @export
format.html_dependency <- function(x, ...) {
  html <- list()

  # add meta content
  if (length(x$meta) > 0) {
    html$meta <- paste(
      "<meta name=\"", names(x$meta), "\" content=\"",
      x$meta, "\" />",
      sep = ""
    )
  }

  # add stylesheets
  if (length(x$stylesheet) > 0) {
    html$stylesheet <- paste(
      "<link href=\"", file.path(x$path, x$stylesheet),
      "\" rel=\"stylesheet\" />",
      sep = ""
    )
  }

  # add scripts
  if (length(x$script) > 0) {
    html$script <- paste(
      "<script src=\"", file.path(x$path, x$script), "\"></script>",
      sep = ""
    )
  }

  # add raw head content
  html$head <- x$head

  unlist(html)
}

copy_deps <- function(dependencies, src = ".", dest = NULL) {
  stopifnot(is.list(dependencies))

  if (is.null(dest)) return()
  if (!file.exists(dest))
    dir.create(dest)

  paths <- unlist(lapply(dependencies, "[[", "path"))
  src_files <- dir(file.path(src, paths), full.names = TRUE, recursive = TRUE)
  dest_files <- sub(src, dest, src_files, fixed = TRUE)

  dest_dirs <- unique(dirname(dest_files))
  lapply(dest_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  file.copy(src_files, dest_files)
}
