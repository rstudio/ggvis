# Convert a data object to a D3-structured data object.
df_to_d3json <- function(x) {
  rows <- nrow(x)
  colnames <- setNames(names(x), names(x))

  x <- lapply(x, format_vec_d3json)

  lapply(seq_len(rows), function(i) {
    lapply(colnames, function(colname) {
      .subset2(.subset2(x, colname), i)
    })
  })
}
