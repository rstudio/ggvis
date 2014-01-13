\name{show_tooltip}
\alias{hide_tooltip}
\alias{show_tooltip}
\title{Send a message to the client to show or hide a tooltip}
\usage{
show_tooltip(session, pagex = 0, pagey = 0, html = "")

hide_tooltip(session)
}
\arguments{
  \item{session}{A Shiny session object.}

  \item{pagex}{x position of the tooltip box on the page.}

  \item{pagey}{y position of the tooltip box on the page.}

  \item{html}{HTML to display in the tooltip box.}
}
\description{
Send a message to the client to show or hide a tooltip
}

