# Custom format for rmarkdown that includes our special sauce in the head
# (can be used as a substitute for html_document until we have the right
# stuff in place in knitr to do this without a special format)
#
# Use within YAML front-matter as follows:
#
# ---
# output: ggvis:::html_document
# ---
#
# You can also include any parameters you'd typically specify
# for html_document:
#
# ---
# output:
#   ggvis:::html_document:
#     theme: united
#     highlight: textmate
# ---
#
html_document <- function(...) {

  # Replace relative path with absolute path
  url_abs_path <- function(path) {
    if (is.null(path)) return(NULL)

    newpath <- system.file(file.path("www", path), package = "ggvis",
                           mustWork = TRUE)
    normalizePath(newpath)
  }

  head <- html_head()

  # Get absolute paths for src/href values
  head[] <- lapply(head, function(tag) {
    if (!is.null(tag$attribs$src)) {
      tag$attribs$src <- url_abs_path(tag$attribs$src)
    }
    if (!is.null(tag$attribs$href)) {
      tag$attribs$href <- url_abs_path(tag$attribs$href)
    }
    tag
  })


  ggvis_head_file <- tempfile("ggvis_head", fileext = ".html")
  cat(format(head), file = ggvis_head_file)

  # delegate to rmarkdown html_document
  rmarkdown::html_document(
    ...,
    self_contained = TRUE,
    includes = rmarkdown::includes(in_header = ggvis_head_file),
  )
}
