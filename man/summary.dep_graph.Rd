% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flowgraph.R
\name{summary.dep_graph}
\alias{summary.dep_graph}
\title{Summary plot of profile information for knitr chunks}
\usage{
\method{summary}{dep_graph}(object, y = "all", ...)
}
\arguments{
\item{object}{Object of class \code{dep_graph}. A dependency graph for a knitr input file.}

\item{y}{Character, default \code{'all'}. Possible values are \code{'all'},
\code{'manual'} and \code{'auto'}, which selects which edges are included for
degree computations.}

\item{...}{Additional arguments, currently ignored.}
}
\value{
A list of ggplot objects.
}
\description{
Visualization of profiling information on chunks obtained from a knitr input
file.
}

