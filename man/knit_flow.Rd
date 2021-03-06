% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flowgraph.R
\name{knit_flow}
\alias{knit_flow}
\title{Knit and extract chunk data}
\usage{
knit_flow(..., cache.path = NULL, fig.path = NULL)
}
\arguments{
\item{...}{All the arguments that should be passed on to \code{knit}}

\item{cache.path}{Character, default \code{NULL}. If set, overrides the default cache path.}

\item{fig.path}{Character, default \code{NULL}. If set, overrides the default figure path.}
}
\value{
An object of class \code{dep_graph}, which inherits class \code{dgr_graph} from
        the DiagrammeR package.
}
\description{
A wrapper function for \code{knit} that gathers and extracts data
from knitr on chunk dependencies to produce a dataflow graph. Additional
profiling data on time usage and cache and figure sizes are extracted
as well.
}
\details{
If a document has already been knitted and rendered in RStudio, say, the arguments
\code{cache.path} and \code{fig.path} can be set to use the existing cache.
}
\seealso{
\code{\link{plot.dep_graph}} and \code{\link[DiagrammeR]{render_graph}} from the
       DiagrammeR package.
}

