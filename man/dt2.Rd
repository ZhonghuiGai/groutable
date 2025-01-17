% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatable2.R
\name{dt2}
\alias{dt2}
\title{A swapped function of DT::datatable to show data frame in Viewer}
\usage{
dt2(data, pageLength = NULL, filter = "none")
}
\arguments{
\item{data}{a data frame}

\item{pageLength}{the length of page}

\item{filter}{a logical value to show the filter option}
}
\value{
a table showed in Viewer
}
\description{
A swapped function of DT::datatable to show data frame in Viewer
}
\examples{
iris  |> dt2()
groutable(data = iris, variable = 5)  |> extract_groutable ()  |> ying_dt()
groutable(data = iris, variable = 5)  |> extract_groutable ()  |> dt2()
}
\author{
Zhonghui Gai
}
