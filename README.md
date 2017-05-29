# Dataflow graphs for knitr

Tools for analyzing the dataflow graph
of a knitr input document based on code chunk dependencies. The
package includes functions for extracting dependencies and other
chunk profiling information, which can be visualized and further analyzed.

## Installation

```
install.packages("devtools")  ## if 'devtools' not installed
devtools::install_github("nielsrhansen/knitrflow", build_vignettes = TRUE)
```

## Usage

The following example provides a simple illustration of how 
the package can be used, and what kind of information it can provide.

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

It's possible to save the graph in the dot file format of [Graphviz](http://www.graphviz.org) and 
then use the command line tools to construct the graph. You then need to have 
[Graphviz installed](http://www.graphviz.org/Download..php). 

```
## Save the graph in the dot format.
write(plot(flow, "manual", plot = FALSE), file = "flow.dot")
## Take a look at the textual representation of the graph.
file.edit("flow.dot")
## Run the following to use the command line tool 'dot' for generating the 
## graph as a png-file.
system("dot -Tpng flow.dot -o flow.png")
## Depending on your OS, view the file. This command works on OS X.
system("open flow.png")
```

## Disclaimer

The package currently relies on the two unexported objects `knit_code`
and `dep_list` from knitr. Thus it relies on implementation details of knitr
that may change.

Knitr doesn't *evaluate* chunks based on the dependencies. The evaluation is 
done in the linear order of the input file. Even if chunks logically could be 
evaluated in a different order according to the graph, this may not be doable 
using knitr. This is primarily due to the fact that objects are shared across all 
chunks during evaluation. 




