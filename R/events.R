#' @include utils.R
NULL

#' The event broker mediates messages between R and JS.
#' 
#' Make a subclass of this reference class if you want to add a new type
#' of interaction to ggvis.
#' 
#' @keywords internal
#' @export
EventBroker <- setRefClass("EventBroker",
  fields = list(
    session = "ANY", 
    path = "ANY", 
    id = "character"
  ),
  methods = list(
    initialize = function(session = NULL, path = NULL, id = rand_id(), ...) {
      initFields(id = id, session = session, path = path, ...)
    },
    
    as_vega = function(...) {
      "This method should return a list suitable for inclusion in the 
      interaction component of the vega specific. The default method sends
      the id, the type (the class name) and the path. If you override it
      to add extra data, make sure to use \\code{callSuper()} to include
      these necessary fields"
      
      class <- as.vector(.refClassDef@className)
      list(id = id, type = class, path = path)
    },
        
    # Standard communication protocols
    message_name = function(name) paste0("ggvis_", id, "_", name),
    
    tell_to = function(name, ...) {
      "Send a message to the client telling it to do something."

      check_session()
      session$sendCustomMessage(message_name(name), list(...))
    },

    listen_for = function(name) {
      "List for a custom message from the client called \\code{name}"
      
      check_session()
      session$input[[message_name(name)]]
    },
    
    check_session = function() {
      if (!is.null(session)) return(TRUE)
      stop("Event broker not initialised with shiny session")
    }
    
  )
)

#' @export
as.vega.envRefClass <- function(x, ...) x$as_vega(...)
