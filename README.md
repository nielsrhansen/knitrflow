# Dataflow graphs for knitr

Tools for analyzing the dataflow graph
of a knitr input document based on code chunk dependencies. The
package includes functions for extracting dependencies and other
chunk profiling information, which can be visualized and further analyzed.

## Installation

```
install.packages("devtools")  ## if 'devtools' not installed
library(devtools)
install_github("nielsrhansen/knitrflow")
```

## Usage

To illustrate the usage of the package, consider the following 
example.

```
library(knitrflow)
example <- system.file("doc/example.Rmd", package = "knitrflow")
flow <- knit_flow(example)
plot(flow)
plot(flow, "manual")
summary(flow)
```

Check out the R Markdown document generating the dependency graph.

```
file.edit(example)
```

### More details

To use the package you need to cache some or all of the chunks in your document.
Whether you rely on manual or automatic detection of dependencies, you just need
to add one chunk at the end of your document with the  `.__grab = TRUE` option set.
Then you can call `knit_flow` as if you were knitting your document. It returns 
the flow graph. 

If you want timings on the chunks, set the option `.__timeit = TRUE`
for the chunks you want timed, or set it as a global option to time all chunks.

An alternative to calling `knit_flow` is to add `knitrflow::set_hooks()` to 
an initial (uncached) chunk and `knitrflow::dataflow_graph(file = "flow.RData")` 
to the last chunk (the one with option `.__grab = TRUE`). Then the flow graph
is stored in a file whenever the document is knitted. It can subsequently be loaded, 
plotted and further analyzed in R. 

### Using Graphviz command line tools

It's possible to save the graph in the dot file format of Graphviz and 
then use the command line tools to construct the graph. You then need to have 
Graphviz installed. 

```
## Save the graph in the dot format.
write(plot(flow, "manual", plot = FALSE), file = "flow.dot")
## Take a look at the textual representation of the graph.
file.edit("flow.dot")
## Run this to use the command line tool 'dot' for generating the 
## graph as a png-file.
system("dot -Tpng flow.dot -o flow.png")
## Depending on your OS, view the file. This command works on OS X.
system("open flow.png")
```

## Disclaimer

The package currently relies on the two unexported objects \code{knit_code} 
and \code{dep_list} from knitr. Thus it relies on implementation details of knitr
that may change.




