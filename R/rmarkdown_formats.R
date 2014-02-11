# Custom html_document and ioslides_presenation formats for rmarkdown
# that includes our special sauce in the head (can be used as a substitute
# for the the formats built into rmarkdown until we have the right stuff
# in place in knitr to do this without a special format)
#
# Use within YAML front-matter as follows:
#
# ---
# output: ggvis:::html_document
# ---
#
# You can also include any parameters you'd typically specify for the
# base format within the rmarkdown package:
#
# ---
# output:
#   ggvis:::ioslides_presentation
#     widescreen: true
# ---
#

html_document <- function(...) {
  rmarkdown::html_document(
    ...,
    self_contained = TRUE,
    includes = rmarkdown_includes()
  )
}

ioslides_presentation <- function(...) {
  rmarkdown::ioslides_presentation(
    ...,
    self_contained = TRUE,
    includes = rmarkdown_includes()
  )
}

rmarkdown_includes <- function() {

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

  rmarkdown::includes(in_header = ggvis_head_file)
}
