#' Send a message to ggvis running on client
#'
#' This will be sent to the client and passed to a handler in ggvis.messages on
#' the client side. The handler is specified by \code{type}.
#'
#' @param session A session object.
#' @param type A string representing the type of the message.
#' @param data An object (typically a list) containing information for the client.
#' @param id A unique identifier for ggvis message handler (optional).
#' @export
ggvis_message <- function(session, type, data = NULL, id = NULL) {
  if (is.null(session)) {
    stop("Need an active Shiny session to send ggvis message")
  }
  session$sendCustomMessage("ggvis_message",
    list(type = type, id = id, data = data))
}
