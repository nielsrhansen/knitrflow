## The functions 'save_objects', 'parse_objects', 'valid_path'
## 'new_defaults' are used in the package but unexported from knitr.
## Their source code is copied from the knitr source.

save_objects = function(objs, label, path) {
  if (length(objs) == 0L) objs = ''
  # save object names
  x = paste(c(label, objs), collapse = '\t')
  if (file.exists(path)) {
    lines = readLines(path)
    lines = lines[lines != label] # knitr < 1.5 may have lines == label
    idx = substr(lines, 1L, nchar(label) + 1L) == paste0(label, '\t')
    if (any(idx)) {
      lines[idx] = x  # update old objects
    } else lines = c(lines, x)
  } else lines = x
  writeLines(lines, con = path)
}

parse_objects = function(path) {
  if (!file.exists(path)) {
    warning('file ', path, ' not found'); return()
  }
  lines = strsplit(readLines(path), '\t')
  if (length(lines) < 2L) return()  # impossible for dependson
  objs = lapply(lines, `[`, -1L)
  names(objs) = lapply(lines, `[`, 1L)
  objs
}

valid_path = function(prefix, label) {
  if (length(prefix) == 0L || is.na(prefix) || prefix == 'NA') prefix = ''
  paste0(prefix, label)
}

new_defaults = function(value = list()) {
  defaults = value

  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else {
        stats::setNames(defaults[name], name)
      }
    }
  }
  set = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) {
    defaults[names(values)] <- values
    defaults
  }
  restore = function(target = value) defaults <<- target

  list(get = get, set = set, merge = merge, restore = restore)
}

## Merge file sizes to node names, format results
file_size <- function(nodes, sizes, glue = "_") {
  node_sizes <- numeric(length(nodes))
  names(node_sizes) <- nodes
  file_names <- names(sizes)
  file_prefix <- paste0(nodes, glue)
  for(i in seq_along(nodes)) {
    node_sizes[nodes[i]] <- sum(sizes[grep(file_prefix[i], file_names)])
  }
  node_sizes
}

## Convert file sizes to suitable unit
format_size <- function(x, digits = 2L) {
  conv <- c(b = 1,
            Kb = 1024,
            Mb = 1024^2,
            Gb = 1024^3,
            Tb = 1024^4)
  units <- function(x) {
    units <- if (x >= conv[5])
      "Tb"
    else if (x >= conv[4])
      "Gb"
    else if (x >= conv[3])
      "Mb"
    else if (x >= conv[2])
      "Kb"
    else "b"
  }
  u <- sapply(x, units)
  cu <- names(conv)[max(which(names(conv) %in% u))]
  x <-  round(x / conv[cu], digits)
  attr(x, "unit") <- cu
  x
}

## Convert timings to suitable unit
format_time <- function(x, digits = 2L) {
  conv <- c(ms = 10^{-3},
            s = 1,
            m = 60,
            h = 3600)
  units <- function(x) {
    units <- if (x >= conv[4])
      "h"
    else if (x >= conv[3])
      "m"
    else if (x >= conv[2])
      "s"
    else "ms"
  }
  u <- sapply(x, units)
  cu <- names(conv)[max(which(names(conv) %in% u))]
  x <-  round(x / conv[cu], digits)
  attr(x, "unit") <- cu
  x
}


## Fixes the start and end format of a HTML-like node in the DOT output
## from DiagrammeR.
replace_html <- function(x) {
  x <- gsub("'__HTML_NODE_START__", "<", x)
  gsub("__HTML_NODE_END__'", ">", x)
}

## Used to remove single quotes from the DOT output from DiagrammeR
## to make it work when using Graphviz command line tools.
remove_quotes <- function(x)
  gsub("'", "", x)
