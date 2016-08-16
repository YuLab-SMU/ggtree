ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data
===========================================================================================================================

[![platform](http://www.bioconductor.org/shields/availability/devel/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#archives) [![Build Status](http://www.bioconductor.org/shields/build/devel/bioc/ggtree.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/ggtree/) [![Linux/Mac Travis Build Status](https://img.shields.io/travis/GuangchuangYu/ggtree/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/GuangchuangYu/ggtree) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/Guangchuangyu/ggtree/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/ggtree) [![install with bioconda](https://img.shields.io/badge/install%20with-bioconda-green.svg?style=flat)](http://bioconda.github.io/recipes/bioconductor-ggtree/README.html)

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![codecov](https://codecov.io/gh/GuangchuangYu/ggtree/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/ggtree) [![Last-changedate](https://img.shields.io/badge/last%20change-2016--08--16-green.svg)](https://github.com/GuangchuangYu/ggtree/commits/master) [![commit](http://www.bioconductor.org/shields/commits/bioc/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#svn_source) [![GitHub forks](https://img.shields.io/github/forks/GuangchuangYu/ggtree.svg)](https://github.com/GuangchuangYu/ggtree/network) [![GitHub stars](https://img.shields.io/github/stars/GuangchuangYu/ggtree.svg)](https://github.com/GuangchuangYu/ggtree/stargazers)

[![releaseVersion](https://img.shields.io/badge/release%20version-1.4.16-green.svg?style=flat)](https://bioconductor.org/packages/ggtree) [![develVersion](https://img.shields.io/badge/devel%20version-1.5.11-green.svg?style=flat)](https://github.com/GuangchuangYu/ggtree) [![Bioc](http://www.bioconductor.org/shields/years-in-bioc/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#since) [![post](http://www.bioconductor.org/shields/posts/ggtree.svg)](https://support.bioconductor.org/t/ggtree/) [![download](http://www.bioconductor.org/shields/downloads/ggtree.svg)](https://bioconductor.org/packages/stats/bioc/ggtree/)

The `ggtree` package extending the `ggplot2` package. It based on grammar of graphics and takes all the good parts of `ggplot2`. `ggtree` is designed for not only viewing phylogenetic tree but also displaying annotation data on the tree.

[![Twitter](https://img.shields.io/twitter/url/https/github.com/GuangchuangYu/ggtree.svg?style=social)](https://twitter.com/intent/tweet?hashtags=ggtree&url=https://guangchuangyu.github.io/ggtree)

------------------------------------------------------------------------

Please cite the following article when using `ggtree`:

**G Yu**, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. ***Methods in Ecology and Evolution***. *accepted*

[![doi](https://img.shields.io/badge/doi-10.1111/2041--210X.12628-green.svg?style=flat)](http://dx.doi.org/10.1111/2041-210X.12628)

------------------------------------------------------------------------

For details, please visit our project website, <https://guangchuangyu.github.io/ggtree>.

-   [Documentation](https://guangchuangyu.github.io/ggtree/documentation/)
-   [FAQ](https://guangchuangyu.github.io/ggtree/faq/)
-   [Featured Articles](https://guangchuangyu.github.io/ggtree/featuredArticles/)
-   [Feedback](https://guangchuangyu.github.io/ggtree/#feedback)

### Download stats

[![download](http://www.bioconductor.org/shields/downloads/ggtree.svg)](https://bioconductor.org/packages/stats/bioc/ggtree/) [![total](https://img.shields.io/badge/downloads-12041/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree/) [![month](https://img.shields.io/badge/downloads-1091/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree/)

         +---------------------------+---------------------------+----------------------------+--------+
         |                                                                                    *        |
    1200 +                                                                                             +
         |                                                                      *                      |
         |                                                                                         *   |
    1000 +                                                                          *    *             +
         |                                                                                             |
         |                                                                                             |
         |                                                                                             |
     800 +                                                  *         *                                +
         |                                                                 *                           |
         |                                              *        *                                     |
     600 +                                         *                                                   +
         |                                                                                             |
         |                                                                                             |
         |                                    *                                                        |
     400 +                           *   *                                                             +
         |                      *                                                                      |
         |                                                                                             |
     200 +                 *                                                                           +
         |                                                                                             |
         |                                                                                             |
         |   *    *   *                                                                                |
       0 +---------------------------+---------------------------+----------------------------+--------+
                                  2015.5                       2016                        2016.5
