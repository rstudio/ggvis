#!/usr/bin/env Rscript

# This script copies the resize detection script from:
#   https://github.com/sdecima/javascript-detect-element-resize
# to the inst/ directory of this package.

# This script can be sourced from RStudio, or run with Rscript.

# Returns the file currently being sourced or run with Rscript
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}

url <- "https://raw.githubusercontent.com/sdecima/javascript-detect-element-resize/master/jquery.resize.js"
destfile <- file.path(dirname(thisFile()), "../inst/www/lib/detect-resize/",
                      basename(url))

downloader::download(url, destfile)
