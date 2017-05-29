#' Computes and saves a dataflow graph
#'
#' An internal function that computes the dataflow graph. It
#' can be used in a knitr chunk to save the graph.
#'
#' @param file Character, default \code{NULL}. File path for saving the dataflow graph.
#'             The graph is not saved by default.
#'
#' @export
dataflow_graph <- function(file = NULL) {
  ## Access to grabbed data
  flow <- knitr::knit_hooks$get(".__grab")
  if (!is.null(flow)) {
    genv <- environment(flow)
    nodes <- genv$nodes
    timings <- genv$timings
    dep_list <- genv$grab_dep$get()
    dep_list_auto <- genv$grab_dep_auto$get()
    objects <- genv$objects
    globals <- genv$globals
    cache_sizes <- genv$cache_sizes
    fig_sizes <- genv$fig_sizes

    ## Transform graph data grabbed from knit
    children <- unlist(dep_list)
    parents <- names(dep_list)
    children_auto <- unlist(dep_list_auto)
    parents_auto <- names(dep_list_auto)
    nodes <- intersect(nodes, c(parents, children, parents_auto, children_auto))
    to <- from <- rel <- edge_label <- vector("list")
    j <- 1
    for(i in parents) {
      ch <- dep_list[[i]]
      k <-  length(ch)
      edge_label[[i]] <- sapply(ch, function(j)
        paste(intersect(globals[[j]], objects[[i]]), collapse = "; "))
      from[[i]] <- rep(which(nodes == i), k)
      to[[i]] <- match(ch, nodes)
      rel[[i]] <- rep("manual", k)
    }

    for(i in parents_auto) {
      if (i %in% parents) {
        ch <- dep_list[[i]]
        ch_auto <- dep_list_auto[[i]]
        ch <- setdiff(ch_auto, ch)
      } else {
        ch <- dep_list_auto[[i]]
      }
      k <- length(ch)
      if(k > 0) {
        edge_label[[i]] <- c(edge_label[[i]],
                             sapply(ch, function(j)
                               paste(intersect(globals[[j]], objects[[i]]), collapse = "; ")))
        from[[i]] <- c(from[[i]], rep(which(nodes == i), k))
        to[[i]] <- c(to[[i]], match(ch, nodes))
        rel[[i]] <- c(rel[[i]], rep("auto", k))
      }
    }

    ec <- c("black", "red")
    names(ec) <- c("manual", "auto")
    from <- unlist(from)
    to <- unlist(to)
    rel <- unlist(rel)
    edge_color <- ec[rel]
    edge_label <- unlist(edge_label)
    times <- sapply(timings$get(), as.numeric)
    if(is.null(times))
      times <- NA
    times <- times[nodes]
    cache_sizes <- file_size(nodes, cache_sizes)
    fig_sizes <- file_size(nodes, fig_sizes, "-")

    objects <- sapply(objects, paste, collapse = "; ")[nodes]
    flow <- DiagrammeR::create_graph(
      DiagrammeR::create_node_df(length(nodes),
                                 label = nodes,
                                 times = times,
                                 sizes = cache_sizes,
                                 fsizes = fig_sizes,
                                 objects = objects),
      DiagrammeR::create_edge_df(from, to, rel, color = edge_color,
                                 label = edge_label),
      attr_theme = NULL
    )
    class(flow) <- c("dep_graph", class(flow))
    if(!is.null(file))
      save(flow, file = file)
  }
  flow
}

#' Knit and extract chunk data
#'
#' A wrapper function for \code{knit} that gathers and extracts data
#' from knitr on chunk dependencies to produce a dataflow graph. Additional
#' profiling data on time usage and cache and figure sizes are extracted
#' as well.
#'
#' If a document has already been knitted and rendered in RStudio, say, the arguments
#' \code{cache.path} and \code{fig.path} can be set to use the existing cache.
#'
#' @param ... All the arguments that should be passed on to \code{knit}
#' @param cache.path Character, default \code{NULL}. If set, overrides the default cache path.
#' @param fig.path Character, default \code{NULL}. If set, overrides the default figure path.
#'
#' @return An object of class \code{dep_graph}, which inherits class \code{dgr_graph} from
#'         the DiagrammeR package.
#' @seealso \code{\link{plot.dep_graph}} and \code{\link[DiagrammeR]{render_graph}} from the
#'        DiagrammeR package.
#' @export
#'
knit_flow <- function(..., cache.path = NULL, fig.path = NULL) {
  if (!is.null(cache.path)) {
    oecp <- TRUE
    old_cp <- knitr::opts_chunk$get("cache.path")
    knitr::opts_chunk$set(cache.path = cache.path)
  } else {
    oecp <- FALSE
  }
  if (!is.null(fig.path)) {
    oefp <- TRUE
    old_fp <- knitr::opts_chunk$get("fig.path")
    knitr::opts_chunk$set(fig.path = fig.path)
  } else {
    oefp <- FALSE
  }

  knit_twice <- FALSE
  if (!file.exists(valid_path(knitr::opts_chunk$get("cache.path"), "__objects")))
    knit_twice <- TRUE

  ## Knit document
  knitr::knit(...)
  ## TODO: Current implementation doesn't give the correct
  ## manual dependencies unless the __objects and __globals
  ## files already exist. Hence the second knitting.
  if (knit_twice)
    knitr::knit(..., quiet = TRUE)

  if (oecp)
    knitr::opts_chunk$set(cache.path = old_cp)
  if (oefp)
    knitr::opts_chunk$set(fig.path = old_fp)

  dataflow_graph()
}

#' Plot knitr dataflow graph
#'
#' Visualization of the dependencies among chunks in a knitr input file.
#' Returns also the graph in the textual dot format (for Graphviz) that can processed
#' using the Graphviz command line tools.
#'
#' @param x    Object of class \code{dep_graph}. A dependency graphs for a knitr input file.
#' @param y    Character, default to \code{'all'}. Possible values are \code{'all'},
#'             \code{'manual'} and \code{'auto'}, which selects which edges types are plotted.
#' @param plot Logical, default to \code{TRUE}. Should the generated plot actually be plotted.
#' @param units Character, default \code{"by_chunk"}. Appropriate units on times and file sizes
#'              are computed individually for each chunk by default. Set to \code{"all_same"} to
#'              get the same unit across all chunks.
#' @param ...   Additional arguments passed on to \code{grViz}.
#'
#' @return A character string (invisibly) containing the graph in the dot format.
#' @export
#'
plot.dep_graph <- function(x, y = 'all', plot = TRUE, units = "by_chunk", ...) {

  if (y == "manual")
    x <- DiagrammeR::delete_edges_ws(
      DiagrammeR::select_edges(x, "rel == 'auto'")
      )
  if (y == "auto")
    x <- DiagrammeR::delete_edges_ws(
      DiagrammeR::select_edges(x, "rel == 'manual'")
      )

  nodes <- x$nodes_df
  if (units == "all_same") {
    times <- format_time(nodes$times)
    times <- paste(times, attr(times, "unit"))
    sizes <- format_size(nodes$sizes)
    sizes <- paste(sizes, attr(sizes, "unit"))
    fsizes <- format_size(nodes$fsizes)
    fsizes <- paste(fsizes, attr(fsizes, "unit"))
  } else {
    times <- sapply(nodes$times, function(t)
    {t <- format_time(t); paste(t, attr(t, "unit"))})
    sizes <- sapply(nodes$sizes, function(s)
    {s <- format_size(s); paste(s, attr(s, "unit"))})
    fsizes <- sapply(nodes$fsizes, function(s)
    {s <- format_size(s); paste(s, attr(s, "unit"))})
  }

  labels <- paste("__HTML_NODE_START__ <TABLE BORDER=\"0\"> <TR>",
                  "<TD ALIGN=\"CENTER\"> <FONT POINT-SIZE = \"40\">",
                  nodes$label,
                  "</FONT> </TD> </TR> <TR> <TD ALIGN=\"LEFT\"> <TABLE BORDER=\"0\">",
                  "<TR> <TD ALIGN=\"LEFT\" WIDTH=\"40%\"> Eval time: </TD> <TD ALIGN=\"LEFT\">",
                  times,
                  "</TD> </TR>",
                  "<TR> <TD ALIGN=\"LEFT\" WIDTH=\"40%\"> Cache size: </TD> <TD ALIGN=\"LEFT\">",
                  sizes,
                  "</TD> </TR>",
                  "<TR> <TD ALIGN=\"LEFT\" WIDTH=\"40%\"> Fig size: </TD> <TD ALIGN=\"LEFT\">",
                  fsizes,
                  "</TD> </TR>",
                  "<TR> <TD ALIGN=\"LEFT\" WIDTH=\"40%\"> Objects: </TD> <TD ALIGN=\"LEFT\">",
                  nodes$objects,
                  "</TD> </TR> </TABLE> </TD> </TR> </TABLE>",
                 "__HTML_NODE_END__")

  x <- DiagrammeR::set_node_attrs(x, "label", labels)

  global_attr <- data.frame(
    attr =      c("layout", "rankdir", "shape", "style"),
    value =     c("dot",    "LR",      "box",   "rounded"),
    attr_type = c("graph",  "graph",   "node",  "node")
  )

  x <- DiagrammeR::add_global_graph_attrs(
    graph = x,
    attr = global_attr$attr,
    value = global_attr$value,
    attr_type = global_attr$attr_type
  )

  x <- DiagrammeR::generate_dot(x)
  x <- replace_html(x)
  if (plot)
    print(DiagrammeR::grViz(x, ...))
  invisible(remove_quotes(x))
}

#' Summary plot of profile information for knitr chunks
#'
#' Visualization of profiling information on chunks obtained from a knitr input
#' file.
#'
#' @param object Object of class \code{dep_graph}. A dependency graph for a knitr input file.
#' @param y    Character, default to \code{'all'}. Possible values are \code{'all'},
#'             \code{'manual'} and \code{'auto'}, which selects which edges are included for
#'             degree computations.
#' @param ...    Additional arguments, currently ignored.
#'
#' @export
#'
summary.dep_graph <- function(object, y = 'all', ...) {
  x <- object

  if (y == "manual")
    x <- DiagrammeR::delete_edges_ws(
      DiagrammeR::select_edges(x, "rel == 'auto'")
    )
  if (y == "auto")
    x <- DiagrammeR::delete_edges_ws(
      DiagrammeR::select_edges(x, "rel == 'manual'")
    )

  nodes <- DiagrammeR::get_node_df(x)
  nodes$n.objects <- sapply(strsplit(nodes$objects, ";"), length)
  nodes <- cbind(DiagrammeR::node_info(x), nodes[, c("times", "sizes", "fsizes", "n.objects")])
  nodes$id <- factor(nodes$id)
  nodes$times <- format_time(nodes$times, digits = 4L)
  nodes$sizes <- format_size(nodes$sizes, digits = 4L)
  nodes$fsizes <- format_size(nodes$fsizes, digits = 4L)
  tmp <- tidyr::gather_(nodes[, c("id", "label", "indeg", "outdeg")],
                       "type", "degree", c("indeg", "outdeg"))
  p1 <- ggplot2::ggplot(nodes, ggplot2::aes_string("rev(id)", "times")) +
    ggplot2::geom_col() + ggplot2::xlab("") +
    ggplot2::ylab(paste("Eval time (", attr(nodes$times, "unit"), ")", sep ="")) +
    ggplot2::scale_x_discrete(labels = rev(nodes$label)) +
    ggplot2::coord_flip()
  p2 <- ggplot2::ggplot(nodes, ggplot2::aes_string("rev(id)", "sizes")) +
    ggplot2::geom_col() + ggplot2::xlab("") +
    ggplot2::ylab(paste("Cache size (", attr(nodes$sizes, "unit"), ")", sep = ""))  +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::coord_flip()
  p3 <- ggplot2::ggplot(nodes, ggplot2::aes_string("rev(id)", "fsizes")) +
    ggplot2::geom_col() + ggplot2::xlab("") +
    ggplot2::ylab(paste("Fig size (", attr(nodes$fsizes, "unit"), ")", sep = ""))  +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::coord_flip()
  p4 <- ggplot2::ggplot(nodes, ggplot2::aes_string("rev(id)", "n.objects")) +
    ggplot2::geom_col() + ggplot2::ylab("Nr of objects") + ggplot2::xlab("") +
    ggplot2::coord_flip() + ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())
  p5 <- ggplot2::ggplot(tmp, ggplot2::aes_string("rev(id)", "degree", fill = "type")) +
    ggplot2::geom_col() + ggplot2::ylab("Degree") + ggplot2::xlab("") +
    ggplot2::coord_flip() + ggplot2::scale_fill_discrete("") +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())

  g1 <- ggplot2::ggplotGrob(p1)
  g2 <- ggplot2::ggplotGrob(p2)
  g3 <- ggplot2::ggplotGrob(p3)
  g4 <- ggplot2::ggplotGrob(p4)
  g5 <- ggplot2::ggplotGrob(p5)

  g <- cbind(g1, g2, g3, g4, g5, size="last")

  grid::grid.newpage()
  grid::grid.draw(g)
  invisible(list(time = p1, cache_size = p2, file_size = p3, objects = p4, degree = p5))
}


