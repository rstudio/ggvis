camelCase <- function(x) {
  words <- strsplit(x, "_")
  
  caps <- lapply(words, function(x) {
    if (length(x) == 1) return(x)
    x[-1] <- capitalise(x[-1])
    x
  })
  
  vapply(caps, paste0, collapse = "", FUN.VALUE = character(1))
}

under_score <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  tolower(gsub("([A-Z])", "_\\1", x))
}

capitalise <- function(x, first = TRUE) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}