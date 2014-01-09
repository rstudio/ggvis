\docType{class}
\name{Keyboard-class}
\alias{Keyboard-class}
\title{Event broker for keyboard events.}
\arguments{
  \item{bindings}{A character vector describing the
  keyboard bindings to listen to. The keyboard event
  handling in ggvis is implemented with
  \href{mousetrap}{http://craig.is/killing/mice} so you can
  specify keys like \code{c("C", "Shift + X", "F2",
  "up"))}}
}
\description{
The hover event broker is useful if you want your shiny app
to respond to keyboard events: \code{key_down},
\code{key_press} and \code{key_up}.
}
\seealso{
\code{\link{left_right}} to easily control values in a
ggvis object with the arrow keys.
}

