#' @S3method + ggvis
#' @rdname ggvis-add
`+.ggvis` <- function(e1, e2){
  e2name <- deparse(substitute(e2))

  switch(component_type(e2),
    children = e1$children <- c(e1$children, list(e2)),
    scales   = e1$scales <- scales(.scales = c(e1$scales, list(e2))),
    legends  = e1$legends <- c(e1$legends, list(e2)),
    axes     = e1$axes <- c(e1$axes, list(e2)),
    props    = e1$props <- merge_props(e1$props, e2),
    opts     = e1$opts <- list(merge_opts(e1$opts[[1]], e2)),
    {
      stop("Don't know how to add object of type ", component_type(e2))
    }
  )

  e1
}
