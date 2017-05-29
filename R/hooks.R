#' Sets knitr hook functions used by knitrflow
#'
#' When called, this function sets the hook functions \code{.__timeit} and
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
  ## Hook for chunk timing
  knitr::knit_hooks$set(
    .__timeit = local({
      now = NULL
      function(before, options) {
        res <- NULL
        if (before) {
          now <<- Sys.time()
        } else {
          res <- difftime(Sys.time(), now, units = "secs")
          path <- valid_path(options$cache.path, '__timings')
          now <<- NULL
          save_objects(res, options$label, path)
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
          nodes <<- names(knitr:::knit_code$get())
          ## Timings
          path <- valid_path(options$cache.path, '__timings')
          timings$set(parse_objects(path))
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
          grab_dep$restore(knitr:::dep_list$get())
          knitr::dep_auto()
          grab_dep_auto$restore(knitr:::dep_list$get())
          ## Objects
          path <- valid_path(options$cache.path, c('__objects', '__globals'))
          objects <<- parse_objects(path[1L])
          globals <<- parse_objects(path[2L])
        }
      }
    })
  )
}
