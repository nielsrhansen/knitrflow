#' knitrflow: A package for knitr dataflow graphs
#'
#' Tools for analyzing the dataflow graph
#' of a knitr input document based on code chunk dependencies. The
#' package includes functions for extracting dependencies and other
#' chunk profiling information, which can be visualized and further analyzed.
#'
#' @details
#'
#' The main function is \code{\link{knit_flow}}. It knits a document and extracts
#' the necessary data for the flow graph.
#'
#' The function \code{\link{dataflow_graph}} can be used inside the last knitr chunk
#' to save the graph instead.
#'
#' When using \code{\link{knit_flow}}, the option \code{.__grab = TRUE} has to
#' be set in a last (extra and empty) chunk, and the optional option \code{.__timeit = TRUE} can be
#' set to time chunks. You can time individual chunks, or set it as a global option
#' to time all chunks.
#'
#' An alternative to calling \code{\link{knit_flow}} is to add \code{knitrflow::set_hooks()} to
#' an initial (uncached) chunk and \code{knitrflow::dataflow_graph(file = "flow.RData")}
#' to the last chunk (the one with option `.__grab = TRUE`). Then the flow graph
#' is stored in a file whenever the document is knitted. It can subsequently be loaded,
#' plotted and further analyzed in R.
#'
#' @docType package
#' @name knitrflow
#' @examples
#'
#' \dontrun{
#' example <- system.file("doc/example.Rmd", package = "knitrflow")
#' flow <- knit_flow(example)
#' plot(flow)
#' plot(flow, "manual")
#' summary(flow)
#' file.edit(example)
#' }
#'
NULL
