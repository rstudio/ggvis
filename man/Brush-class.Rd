\docType{class}
\name{Brush-class}
\alias{Brush-class}
\title{Event broker for brush events.}
\description{
Event broker for brush events.
}
\section{Methods}{
\itemize{
\item \code{as_vega(...)}: This method should return a list suitable for inclusion in the 
interaction component of the vega specific. The default method sends
the id, the type (the class name) and the path. If you override it
to add extra data, make sure to use \code{callSuper()} to include
these necessary fields

\item \code{brush_move()}: A reactive value that changes every time the brush is resized or moved.
Returns a list containing:
  * plot_id: The ID of the ggvis plot.
  * x1, x2, y1, y2: Pixel coordinates of the brushed region, relative to
    plotting area.
  * pagex1, pagex2, pagey1, pagey2: Pixel coordinates of brushed region,
    relative to web page.
  * items: a list with one item per mark. Each item is another list
    containing one entry for each variable in the data, as well as a field
    named `key__`.

\item \code{listen_for(name)}: List for a custom message from the client called \code{name}
}}

