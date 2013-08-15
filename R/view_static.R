#' Generate a static web page with the embedded ggvis graph
#'
#' @param gv A ggvis object.
#' @param renderer The renderer to use in the browser. Can be \code{"canvas"}
#'   (the default) or \code{"svg"}.
#' @param launch If \code{TRUE}, launch this web page in a browser.
#'
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
#' @keywords internal
view_static <- function(gv, renderer = "canvas", launch = TRUE) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  temp_dir <- tempfile(pattern = "ggvis")
  dir.create(temp_dir)

  copy_www_resources(temp_dir)

  spec <- as.vega(gv, dynamic = FALSE)
  vega_json <- toJSON(spec, pretty = TRUE)

  template <- paste(readLines(system.file('index.html', package='ggvis')),
    collapse='\n')

  js <- paste0(
    '<script type="text/javascript">
      function parse(spec) {
        vg.parse.spec(spec, function(chart) {
          view = chart({el:"#vis", renderer: "', renderer, '"}).update();
        });
      }
      parse(', vega_json, ');
    </script>')

  body <- paste('<div id="vis"></div>', js, sep ='\n')

  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker.render(template, list(head = '', body = body)),
    con = html_file)

  browseURL(html_file)
}


#' Generate an PNG file from a ggvis object
#'
#' This requires that the program \code{vg2png} is installed. This is part of
#' \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2png}.
#'
#' @param gv A ggvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param file Output file name. If NULL, defaults to "plot.png".
#' @export
vega_png <- function(gv, envir = parent.frame(), file = NULL) {
  vega_file(gv, envir, file = file, type = "png")
}

#' Generate a SVG file from a ggvis object
#'
#' This requires that the program \code{vg2png} is installed. This is part of
#' \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2svg}.
#'
#' @param gv A ggvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param file Output file name. If NULL, defaults to "plot.svg".
#' @export
vega_svg <- function(gv, envir = parent.frame(), file = NULL) {
  vega_file(gv, envir, file = file, type = "svg")
}

# Generate an output image file from a ggvis object
#
# @param gv A ggvis object.
# @param envir The environment in which to evaluate \code{gv}.
# @param file Output file name. If NULL, defaults to "plot.png".
# @param type Output file type.
vega_file <- function(gv, envir = parent.frame(), file = NULL,
                      type = "png") {

  if (!(type %in% c("png", "svg")))  stop("type must be 'png' or 'svg'")

  if (is.null(file))  file <- paste0("plot.", type)

  temp_dir <- tempfile(pattern = "ggvis")
  dir.create(temp_dir)

  # Try to find the external program that generates the images
  cmd <- paste0("vg2", type)

  # Search in these paths and use the first one found
  cmdsearch <- Sys.which(paste0(c("", "./bin/", "./node_modules/.bin/"), cmd))
  found_idx <- which(nzchar(cmdsearch))
  if (length(found_idx) == 0)
    stop("Conversion program ", cmd, "not found.")
  cmd <- cmdsearch[min(found_idx)]

  # Generate the Vega JSON spec
  json_file <- file.path(temp_dir, "plot.json")
  vega_json <- save_spec(json_file, gv)
  on.exit(unlink(json_file))

  # Create the image file
  system2(cmd, args = c(json_file, file))
}

copy_www_resources <- function(destdir) {
  files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js"
  )

  lapply(files, function(file) {
    src <- system.file("www", file, package = "ggvis")

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir)

    file.copy(src, destfile)
  })
}
