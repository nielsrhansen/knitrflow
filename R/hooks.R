#' Sets knitr hook functions used by knitrflow
#'
#' When called, this function sets the hook functions \code{.__flow} and
#' \code{.__grab}. It is called when the
#' knitrflow package is attached, thus these hooks are set whenever the package
#' is loaded.
#'
#' It can be used in a knitr document to set the hooks without loading knitrflow,
#' e.g. to make sure that the hook functions are set no matter how
#' the document is knitted.
#'
#' @export
#'
set_hooks <- function() {
  ## Hook for chunk dependency tracking and timing
  knitr::knit_hooks$set(
    .__flow = local({
      now = NULL
      function(before, options) {
        res <- NULL
        if (before) {
          if (!dir.exists(options$cache.path))
            dir.create(options$cache.path)
          now <<- Sys.time()
        } else {
          res <- difftime(Sys.time(), now, units = "secs")
          path <- valid_path(options$cache.path, '__timings')
          now <<- NULL
          save_objects(res, options$label, path)
          path <- valid_path(options$cache.path, '__depend')
          save_objects(options$dependson, options$label, path)
        }
      }
    })
  )

  ## Hook for grabbing
  knitr::knit_hooks$set(
    .__grab = local({
      timings <- new_defaults()
      grab_dep <- new_defaults()
      grab_dep_auto <- new_defaults()
      nodes <- character()
      objects <- character()
      globals <- character()
      cache_sizes <- vector()
      fig_sizes <- vector()
      function(before, options) {
        if (before) {
          ## Timings
          path <- valid_path(options$cache.path, '__timings')
          timings$restore(parse_objects(path))
          ## Cache sizes
          path <- options$cache.path
          files <- dir(path)
          cache_sizes <<- file.size(paste0(path, files))
          names(cache_sizes) <<- files
          ## Figure sizes
          path <- options$fig.path
          files <- dir(path)
          fig_sizes <<- file.size(paste0(path, files))
          names(fig_sizes) <<- files
          ## Dependency graph
          path <- valid_path(options$cache.path, '__depend')
          grab_dep$restore(parse_objects(path))
          nodes <<- names(knitr:::knit_code$get())
          ## The solution above relies on an unexported object from knitr.
          ## TODO: Is it possible to extract the correctly ordered chunk labels
          ## from the current document in another way?
          ## The solution below does not use internal knitr objects, but
          ## it doesn't know what chunks are in the actual current document. Thus it
          ## uses all information cached, and order chunks according to
          ## cache ordering.
          ## nodes <<- names(grab_dep$get())
          grab_dep_auto$restore(list())
          dep_auto(nodes, grab_dep_auto, options$cache.path)
          ## Objects
          path <- valid_path(options$cache.path, c('__objects', '__globals'))
          objects <<- parse_objects(path[1L])
          globals <<- parse_objects(path[2L])
        }
      }
    })
  )
}
