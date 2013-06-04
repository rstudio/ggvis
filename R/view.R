#' Generate a static web page with the embedded gigvis graph
#'
#' @param gv A gigvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param renderer The renderer to use in the browser. Can be \code{"canvas"}
#'   (the default) or \code{"svg"}.
#' @param launch If \code{TRUE}, launch this web page in a browser.
#'
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
view_static <- function(gv, envir = parent.frame(), renderer = "canvas",
                        launch = TRUE) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  temp_dir <- tempfile(pattern = "gigvis")
  dir.create(temp_dir)

  copy_www_resources(temp_dir)

  vega_json <- toJSON(vega_spec(gv, envir = envir), pretty = TRUE)

  template <- paste(readLines(system.file('index.html', package='gigvis')),
    collapse='\n')

  js <- paste0(
    '<script type="text/javascript">
      function parse(spec) {
        vg.parse.spec(spec, function(chart) {
          chart({el:"#vis", renderer: "', renderer, '"}).update();
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


#' Generate an PNG file from a gigvis object
#'
#' This requires that the program \code{vg2png} is installed. This is part of
#' \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2png}.
#'
#' @param gv A gigvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param file Output file name. If NULL, defaults to "plot.png".
#' @export
vega_png <- function(gv, envir = parent.frame(), file = NULL) {
  vega_file(gv, envir, file = file, type = "png")
}

#' Generate a SVG file from a gigvis object
#'
#' This requires that the program \code{vg2png} is installed. This is part of
#' \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2svg}.
#'
#' @param gv A gigvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param file Output file name. If NULL, defaults to "plot.svg".
#' @export
vega_svg <- function(gv, envir = parent.frame(), file = NULL) {
  vega_file(gv, envir, file = file, type = "svg")
}

# Generate an output image file from a gigvis object
#
# @param gv A gigvis object.
# @param envir The environment in which to evaluate \code{gv}.
# @param file Output file name. If NULL, defaults to "plot.png".
# @param type Output file type.
vega_file <- function(gv, envir = parent.frame(), file = NULL,
                      type = "png") {

  if (!(type %in% c("png", "svg")))  stop("type must be 'png' or 'svg'")

  if (is.null(file))  file <- paste0("plot.", type)

  temp_dir <- tempfile(pattern = "gigvis")
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
  vega_json <- toJSON(vega_spec(gv, envir = envir), pretty = TRUE)
  json_file <- file.path(temp_dir, "plot.json")
  cat(vega_json, file = json_file)
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
    src <- system.file("www", file, package = "gigvis")

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir)

    file.copy(src, destfile)
  })
}
