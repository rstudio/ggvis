# Convert a data object to a D3-structured data object.
# Numbers and strings stay the same type; everything else gets converted to
# strings with as.character().
d3df <- function(x) UseMethod("d3df")

#' @S3method d3df data.frame
d3df.data.frame <- function(x) {
  rows <- nrow(x)
  colnames <- setNames(names(x), names(x))
  
  x <- lapply(x, function(col) {
    if (is.numeric(col) || is.character(col))  col
    else  as.character(col)
  })
  
  lapply(seq_len(rows), function(i) {
    lapply(colnames, function(colname) {
      .subset2(.subset2(x, colname), i)
    })
  })
}

#' @S3method d3df split_df
d3df.split_df <- function(x) {
  unlist(lapply(x, d3df), recursive = FALSE)
}
