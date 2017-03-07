<!-- addtoany:= -->


<img src="https://raw.githubusercontent.com/jotsetung/BioC-stickers/master/ggtree/ggtree.png" height="128", align="right" />

<link rel="stylesheet" href="https://guangchuangyu.github.io/css/font-awesome.min.css">

<!-- release:=ggtree -->
<!-- devel:=ggtree -->
<!-- download:=ggtree:=total -->
<!-- download:=ggtree:=month -->


The `ggtree` package extending the *ggplot2* package. It based on grammar of graphics and takes all the good parts of *ggplot2*.  *ggtree* is designed for not only viewing phylogenetic tree but also displaying annotation data on the tree.
*ggtree* is released within the [Bioconductor](https://bioconductor.org/packages/ggtree/) project and the source code is hosted on <a href="https://github.com/GuangchuangYu/ggtree"><i class="fa fa-github fa-lg"></i> GitHub</a>.


## <i class="fa fa-user"></i> Authors

Guangchuang Yu and Tommy Tsan-Yuk Lam, School of Public Health, The University of Hong Kong.

## <i class="fa fa-book"></i> Citation

Please cite the following article when using `ggtree`:

<!-- doi:=10.1111/2041-210X.12628 -->
<!-- citation:=HtEfBTGE9r8C:=7268358477862164627 -->
<!-- altmetric:=10533079 -->

__G Yu__, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. __*Methods in Ecology and Evolution*__. 2017, 8(1):28-36.

## <i class="fa fa-pencil"></i> Featured Articles

![](https://guangchuangyu.github.io/featured_img/ggtree/2015_peiyu_1-s2.0-S1567134815300721-gr1.jpg)

<i class="fa fa-hand-o-right"></i> Find out more on <i class="fa fa-pencil"></i> [Featured Articles](https://guangchuangyu.github.io/ggtree/featuredArticles/).

## <i class="fa fa-download"></i> Installation

Install `ggtree` is easy, follow the guide on the [Bioconductor page](https://bioconductor.org/packages/ggtree/):

```r
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
## biocLite("BiocUpgrade") ## you may need this
biocLite("ggtree")
```

If you have problems when installing some of the dependent packages, please refer to the [ggtree-installation](https://github.com/GuangchuangYu/ggtree/wiki/ggtree-installation) wiki page.

## <i class="fa fa-cogs"></i> Overview

#### <i class="fa fa-angle-double-right"></i> Getting tree into R

+ tree parsers: bring evolution evidences to be used/analyzed in `R`
+ `merge_tree`: allows evolution evidences to be merged and compared
+ `fortify` methods: convert tree objects into tidy data frame

#### <i class="fa fa-angle-double-right"></i> Tree visualization & annotation

+ parsing tree as a collection of nodes allows grammar of graphics to be supported
+ `geom_tree`: extends `ggplot2` to support tree structure
+ several layers and functions for tree annotation
+ supports annotating phylogenetic trees with user's own data

#### <i class="fa fa-angle-double-right"></i> Tree manipulation

+ helper functions for tree manipulation, make it possible to explore the tree visually

<i class="fa fa-hand-o-right"></i> Find out details and examples on <i class="fa fa-book"></i> [Documentation](https://guangchuangyu.github.io/ggtree/documentation/).




## <i class="fa fa-code-fork"></i> Projects that depend on _ggtree_

<!-- package_depend:=ggtree:=CRAN -->

<!-- package_depend:=ggtree:=BioC -->

#### <i class="fa fa-angle-double-right"></i> Other applications

+ [BreadCrumbs](https://bitbucket.org/biobakery/breadcrumbs): Collection of scripts for metagenomics analysis
+ [DegeneratePrimerTools](https://github.com/esnapd/DegeneratePrimerTools): Utilities for Creating and Validating Degenerate primers
+ [phyloscan](https://github.com/olli0601/phyloscan): scan phylogenies created along a genome for patterns


## <i class="fa fa-comments"></i> Feedback

<ul class="fa-ul">
	<li><i class="fa-li fa fa-hand-o-right"></i> Please make sure you have followed <a href="https://guangchuangyu.github.io/2016/07/how-to-bug-author/"><strong>the important guide</strong></a> before posting any issue/question</li>
	<li><i class="fa-li fa fa-bug"></i> For bugs or feature requests, please post to <i class="fa fa-github-alt"></i> <a href="https://github.com/GuangchuangYu/ggtree/issues">github issue</a></li>
	<li><i class="fa-li fa fa-question"></i>  For user questions, please post to <i class="fa fa-google"></i> <a href="https://groups.google.com/forum/#!forum/bioc-ggtree">google group</a></li>
	<li><i class="fa-li fa fa-support"></i> We are also following every post tagged with <strong>ggtree</strong> on <a href="https://support.bioconductor.org">Bioconductor support site</a> and <a href="https://www.biostars.org">Biostars</a></li>
	<li><i class="fa-li fa fa-commenting"></i> Join the group chat on <a href="https://twitter.com/hashtag/ggtree"><i class="fa fa-twitter fa-lg"></i></a> and <a href="http://huati.weibo.com/k/ggtree"><i class="fa fa-weibo fa-lg"></i></a></li>
</ul>


