# <i class="fa fa-download"></i> Installation

## <i class="fa fa-angle-double-right"></i> Could not find function 

If you got [this error](https://github.com/GuangchuangYu/ggtree/issues/12), please make sure you are using the latest R and `ggtree`. 

Packages in Bioconductor, like `ggtree`, have different release policy compare to CRAN. There are two branches, release and devel, in parallel. Release branch is more stable and only document improvement and bug fixes will commit to it. New functions will only commit to `devel` branch.

Sometimes I may write blog post to introduce new functions which is not available in `release` branch, you need to install the `devel` version of `ggtree` in order to use these new functions.

You can download the `devel` version of `ggtree` from [http://bioconductor.org/packages/devel/bioc/html/ggtree.html](http://bioconductor.org/packages/devel/bioc/html/ggtree.html) and install it, or install the github version of `ggtree`.

This also applied to other of my packages, including `GOSemSim`, `DOSE`, `clusterProfiler`, `ReactomePA` and `ChIPseeker`. If you got the `could not find function` error, upgrade your installation to latest release. If the error still exists after upgrading to latest release, you need to install the devel version.


# <i class="fa fa-text-height"></i> Text & Label

## <i class="fa fa-angle-double-right"></i> Tip label truncated

ggplot2 can't auto adjust xlim based on added text. 

```r
library(ggtree)
## example tree from https://support.bioconductor.org/p/72398/
tree<-read.tree(text="(Organism1.006G249400.1:0.03977,(Organism2.022118m:0.01337,(Organism3.J34265.1:0.00284,Organism4.G02633.1:0.00468)0.51:0.0104):0.02469);")
ggtree(tree) + geom_tiplab()
```

This is because the units are in two different spaces (data and pixel). Users can use xlim to allocate more space for tip label.

```r
ggtree(tree) + geom_tiplab() + xlim(0, 0.06)
```

## <i class="fa fa-angle-double-right"></i> Formatting (tip) labels

If you want to format labels, you need to set `parse=TRUE` in `geom_text`/`geom_tiplab` and the `label` should be string that can be parsed into expression and displayed as described in `?plotmath`.

For example, the tiplabels contains two parts, species name and accession number and we want to display species name in _italic_, we can use command like this:

```
ggtree(rtree(30)) + geom_tiplab(aes(subset=node==35), label='paste(italic("species name"), "accession number")', parse=T)
```

## <i class="fa fa-angle-double-right"></i> Avoid overlapping text labels

User can use [ggrepel](https://cran.r-project.org/web/packages/ggrepel/) package to repel overlapping text labels.

For example:

```r
library(ggrepel)
library(ggtree)
raxml_file <- system.file("extdata/RAxML", "RAxML_bipartitionsBranchLabels.H3", package="ggtree")
raxml <- read.raxml(raxml_file)
ggtree(raxml) + geom_label_repel(aes(label=bootstrap, fill=bootstrap))
```

For details, please refer to [ggrepel usage examples](https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html).


# <i class="fa fa-map-marker"></i> _aesthetic_ mapping

## <i class="fa fa-angle-double-right"></i> inherit _aes_

```r
ggtree(rtree(30)) + geom_point()
```

For example, we can add symbolic points to nodes with `geom_point()` directly. 
The magic here is we don't need to map `x` and `y` position of the points by providing `aes(x, y)` to `geom_point()` since it was already mapped by `ggtree` function and it serves as a global mapping for all layers. 

But what if we provide a `dataset` in a layer and the `dataset` doesn't contain column of `x` and/or `y`, 
the layer function also try to map `x` and `y` and also others if you map them in `ggtree` function. 
As these variable is not available in your `dataset`, you will get the following error:
 
```
Error in eval(expr, envir, enclos) : object 'x' not found
```

This can be fixed by using parameter `inherit.aes=FALSE` which will disable inheriting mapping from `ggtree` function.


