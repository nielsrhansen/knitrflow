# Dataflow graphs for knitr

Tools for analyzing the dataflow graph
of a knitr input document based on code chunk dependencies. The
package includes functions for extracting dependencies and other
chunk profiling information, which can be visualized and further analyzed.

![Flow](https://nielsrhansen.github.com/img/flow.png)

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
example <- system.file("Rmd/example.Rmd", package = "knitrflow")
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

To use the package you just need to add one chunk at the end of your 
document with the  `.__grab = TRUE` and `cache = FALSE` options set. Then you can call `knit_flow` 
as if you were knitting your document. However, the result is a pretty boring 
empty graph. To get something interesting set `.__flow = TRUE` either for the 
chunks you want to track and time or as a global option to track and time all chunks.
You should also set the option `cache = TRUE` either globally or at least
for the chunks you are tracking.  

Whether you rely on manual or automatic detection of dependencies, `knit_flow` 
will return the deduced flow graph. If the global option `autodep = TRUE` is set,
all dependencies that are auto-detected, but not manually set via `dependson`, are
marked as `auto`. Other edges are marked as `manual`. The auto-detected edges 
are marked by red on the plot of the graph. An additional bonus from setting `autodep = TRUE`
is that edges and nodes are equipped with explicit information on object dependencies. 

An alternative to calling `knit_flow` is to add `knitrflow::set_hooks()` to 
an initial (uncached) chunk and `knitrflow::dataflow_graph(file = "flow.RData")` 
to the last chunk (the one with option `.__grab = TRUE`, which must be uncached 
as well). Then the flow graph
is stored in the file `flow.RData` whenever the document is knitted. It can subsequently be loaded, 
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

## How to read the graph

Knitr doesn't **evaluate** chunks based on the dependencies. The evaluation is 
done in the linear order of the input file. Even if chunks logically could be 
evaluated in a different order according to the graph, this may not be doable 
using knitr. This is primarily due to the fact that variables (objects) are shared across all 
chunks during evaluation. Thus changing the actual evaluation order could inadvertently 
result in variables being overwritten by intermediate chunks. 

The manually set `dependson` chunk options control the reevaluation of cached chunks. 
When these options are set correctly and in a minimal way, the graph will reflect the 
actual evaluation dependencies, and thus the dataflow. The graph of the `manual`
edges only reflects how the author perceives the evaluation dependencies, but 
it may provide a useful overview. 

The `auto` edges are obtained using a function derived from knitr's `dep_auto`. It's based 
on a chunk's usage of non-local objects, and where such objects could have been defined in 
other chunks. It's conservative, and may introduce many **potential** dependencies that are 
not actual dependencies. This is particularly so, if a variable name is reused and 
reassigned in many chunks. In summary, a red edge is potentially a dependency that was 
missed by the manually set `dependson` options, but it need not be. 









