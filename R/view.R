#' Generate a static web page with the embedded gigvis graph
#'
#' @param gv A gigvis object.
#' @param launch If TRUE, launch this web page in a browser.
#'
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
view_static <- function(gv, envir = parent.frame(), launch = TRUE) {

  temp_dir <- tempfile(pattern = "gigvis")
  dir.create(temp_dir)

  copy_www_resources(temp_dir)

  vega_json <- toJSON(vega_spec(gv, envir = envir), pretty = TRUE)

  template <- paste(readLines(system.file('index.html', package='gigvis')),
    collapse='\n')

  js <- paste0(
    '<script type="text/javascript">
      function parse(spec) {
        vg.parse.spec(spec, function(chart) { chart({el:"#vis"}).update(); });
      }
      parse(', vega_json, ');
    </script>')

  body <- paste('<div id="vis"></div>', js, sep ='\n')

  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker.render(template, list(head = '', body = body)),
    con = html_file)

  browseURL(html_file)
}


copy_www_resources <- function(destdir) {
  files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js"
  )

  lapply(files, function(file) {
    src <- system.file("www", file, package = "gigvis")

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir)

    file.copy(src, destfile)
  })
}
