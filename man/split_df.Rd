\name{split_df}
\alias{split_df}
\title{Create a data frame split up by specified variables}
\usage{
  split_df(data, split)
}
\description{
  Create a data frame split up by specified variables
}
\examples{
split_df(mtcars, NULL)
split_df(mtcars, "cyl")
split_df(mtcars, c("cyl", "vs"))
split_df(split_df(mtcars, "cyl"), "vs")
}

