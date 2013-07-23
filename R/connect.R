#' Connect a pipeline creating a reactive expression.
#'
#' \code{connect} creates a reactive pipeline (starting with \code{NULL}),
#' applying each pipe (transformation) in sequence. \code{sluice} is a
#' convenient helper method that creates the pipeline, then flows the data
#' along it to render a final result.
#'
#' Every element in a pipeline recieves the same set of properties: this
#' generally means that for more complicated transformations you will need
#' to create multiple branches.
#'
#' @param x a pipeline or pipe
#' @param properties a \code{props} object
#' @param data the data source to start the connect. May be reactive.
#' @export
#' @keywords internal
#' @examples
#' df <- data.frame(x = 1:10, y = 1:10)
#' asis <- props(x ~ x, y ~ y)
#'
#' sluice(transform_scale(), asis, df)
#' sluice(transform_scale(mult = -1), asis, df)
#' sluice(transform_scale(add = 10), asis, df)
#'
#' # Length two pipeline
#' p <- pipeline(transform_scale(mult = -1), transform_scale(mult = -1))
#' sluice(p, asis, df)
#' p <- pipeline(transform_scale(mult = 2), transform_scale(mult = 2))
#' sluice(p, asis, df)
#'
#' # Reactive pipeline
#' v <- reactiveValues(add = 0, mult = 1)
#' ts <- transform_scale(add = reactive(v$add), mult = reactive(v$mult))
#' r <- connect(ts, asis, df)
#' isolate(r())
#' v$add <- 2
#' isolate(r())
connect <- function(x, props, data = NULL) {
  stopifnot(is.gigvis_props(props))
  needs_shiny()

  UseMethod("connect")
}

#' @rdname connect
sluice <- function(x, props, data) {
  needs_shiny()
  isolate(connect(x, props, data)())
}

#' @S3method connect pipeline
connect.pipeline <- function(x, props, data = NULL) {
  data <- as.reactive(data)

  connect_pipe <- function(pipe, props, data) {
    force(data)
    force(pipe)
    connect(pipe, props, data())
  }

  for (pipe in x) {
    data <- connect_pipe(pipe, props, data)
  }

  data
}

#' @S3method connect NULL
connect.NULL <- function(x, data, props) {
  as.reactive(data)
}
