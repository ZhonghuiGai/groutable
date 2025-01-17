% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_groutable.R
\name{extract_groutable_descr}
\alias{extract_groutable_descr}
\title{Transform the result of compareGroups to a data frame for further use}
\usage{
extract_groutable_descr(groutable)
}
\arguments{
\item{groutable}{the result of createTable in compareGroups package}
}
\value{
a data frame
}
\description{
Transform the result of compareGroups to a data frame for further use
}
\examples{
groutable(data = iris, variable = 5)  |> extract_groutable ()
}
\author{
Zhonghui Gai
}
