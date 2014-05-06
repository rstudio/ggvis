handle_hover <- function(vis, on_mouse_over = NULL, on_mouse_out = NULL) {
  check_callback(on_mouse_over, c("value", "session"))
  check_callback(on_mouse_out, c("value", "session"))

  connect <- function(session, plot_id) {
    setup_callback(on_mouse_over, paste0(plot_id, "_mouse_over"), session)
    setup_callback(on_mouse_out, paste0(plot_id, "_mouse_out"), session)
  }

  spec <- list(type = "hover")
  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)
  register_reactive(vis, broker)
}

