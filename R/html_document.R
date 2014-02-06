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

  absolute_paths <- function(x) {
    if (inherits(x, "shiny.tag.list")) {
      x[] <- lapply(x, absolute_paths)
      return(x)
    }

    if (inherits(x, "shiny.tag")) {
      if (x$name == "script") {
        x$attribs$src <- url_abs_path(x$attribs$src)
      } else if (x$name == "link") {
        x$attribs$href <- url_abs_path(x$attribs$href)
      } else {
        stop("don't know what to do with tag with name ", x$name)
      }
      return(x)
    }

    stop("don't know what to do with ", x)
  }


  js_stuff <- tagList(
    tags$script(src = "lib/jquery-1.9.1.js"),
    tags$script(src = "lib/jquery-ui/js/jquery-ui-1.10.3.custom.js"),
    tags$script(charset = "utf-8", src = "lib/d3.min.js"),
    tags$script(src = "lib/vega.js"),
    tags$script(src = "lib/QuadTree.js"),
    tags$script(src = "lib/lodash.min.js"),
    tags$script("var lodash = _.noConflict()"),
    tags$script(src = "js/ggvis.js"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.3.custom.css"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "css/ggvis.css")
  )

  script_text <- absolute_paths(js_stuff)

  ggvis_head_file <- tempfile("ggvis_head", fileext = ".html")
  cat(format(script_text), file = ggvis_head_file)

  # delegate to rmarkdown html_document
  rmarkdown::html_document(
    ...,
    includes = rmarkdown::includes(in_header = ggvis_head_file),
  )
}
