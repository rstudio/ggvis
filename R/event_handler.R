if(F) {
# Eventually want to scope an interactive to a ggvis branch and its children
# So all Interactives also have a path argument. In javascript, should only
# trigger (or respond to) events from marks that (partially) match the path.

# Always created in reactive context
EventBroker <- setRefClass("EventBroker",
  fields = list(
    id = "character",
    "session" = NULL
  ),
  methods = list(
    initialize = function(session, id = NULL) {
      id <<- id %||% random_id()
      session <<- session
      register_client_handler()
    },

    # TODO: replace this with as.vega method so that the information is embedded
    # in the spec. On the Javascript side, when spec is read, set up appropriate
    # event handlers. Also wouldnt hurt for JS side to accept these custom messages.
    # register_client_handler = function() {
    #   # Tells JS to set up events listeners which call Shiny.onInputChange
    #   # And addCustomMessageHandler to listen to custom events
    #   session$sendCustomMessage("register_event_handler",
    #     list(id = id, type = class(self)[1])
    #   )
    # },


    # Standard communication protocols
    message_name <- function(name) paste0("ggvis_", id, "_", name),
    
    tell_to = function(name, ...) {
      check_session()
      sendCustomMessage(message_name(name), list(...))
    },
    listen_for = function(name) {
      check_session()
      input[[message_name(name)]]
    }
  )
)

Hover <- setRefClass("Hover", contains = "Interaction",
  methods = list(
    mouse_out = function() listen_for("mouse_out"),
    mouse_over = function() listen_for("mouse_over")
  )
)

# tooltip <- function(f) {
#   stopifnot(is.function(f))
  
#   structure(list(f = f), class = c("tooltip", "input"))
# }
# as.reactive.tooltip <- function(x, session, ...) {
#   h <- Hover(session)
#   observe({
#     h$mouse_out()
#     h$tell_to("hide_tooltip")
#   })
#   observe({
#     hover <- h$mouse_over()
#     html <- fun(hover$data)
    
#     tell_to("show_tooltip",
#       visible = TRUE,
#       pagex = hover$pagex - 100,
#       pagey = hover$pagey + 6,
#       html = html
#     )
#   })
#   reactive(NULL)
# }

# Keyboard <- setRefClass("Keyboard", contains = "Interaction",
#   # Characer of keyboard bindings to listen to: c("C", "Shift + X", "F2", "up"))
#   fields = list("bindings" = character()),
#   methods = list(
#     key_down = function() listen_for("key_down"),
#     key_press = function() listen_for("key_press"),
#     key_up = function() listen_for("key_up"),
#     register_client_handler = function() {
#       session$sendCustomMessage("register_event_handler",
#         list(id = id, type = class(self)[1], listen_for = bindings
#       )
#     },
#   )
# )

# left_right <- function(min, max, value = min, step = (max - min) / 50) {
#   structure(list(min = min, max = max, value = value, step = step),
#     class = c("left_right", "input")
# }

# as.reactive.left_right <- function(x, session, ...) {
#   k <- Keyboard(session, c("left", "right"))
  
#   i <- x$value
#   reactive({
#     # Not sure how default value detected for first run
#     press <- k$key_press()
#     if (press == "left"  && i > x$min) i <<- i - x$step
#     if (press == "right" && i < x$max) i <<- i + x$step
    
#     i
#   })
# }
}