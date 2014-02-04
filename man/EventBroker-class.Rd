\docType{class}
\name{EventBroker-class}
\alias{EventBroker-class}
\title{The event broker mediates messages between R and JS.}
\description{
Make a subclass of this reference class if you want to add
a new type of interaction to ggvis.
}
\section{Methods}{
\itemize{
\item \code{as_vega(...)}: This method should return a list suitable for inclusion in the 
interaction component of the vega specific. The default method sends
the id, the type (the class name) and the path. If you override it
to add extra data, make sure to use \code{callSuper()} to include
these necessary fields

\item \code{listen_for(name)}: List for a custom message from the client called \code{name}
}}
\keyword{internal}

