\name{band}
\alias{band}
\alias{is.band}
\title{A band}
\usage{
band(offset = NULL, mult = NULL)

is.band(x)
}
\arguments{
  \item{offset,mult}{Additive and multiplicate offsets used
  to adjust the band size. For example, use \code{mult =
  0.9} to make a bar take up 90% of the space allocated for
  its category.}

  \item{x}{object to for band-ness}
}
\description{
Bands are used to set the width or height on categorical
scales - a band represent the height or width allocated for
one level of a factor.
}
\examples{
df <- data.frame(label = c("a", "b", "c"), n = c(10, 9 , 4))

base <- ggvis(df, props(x = ~label, y2 = 0, y := ~n)) + mark_rect()
base + props(width := band())
base + props(width := band(offset = -1))
base + props(width := band(mult = 0.9))
}

