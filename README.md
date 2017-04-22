<!-- README.md is generated from README.Rmd. Please edit that file -->
ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data
===========================================================================================================================

<img src="https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/ggtree/ggtree.png" height="200" align="right" />

[![](https://img.shields.io/badge/release%20version-1.6.11-green.svg?style=flat)](https://bioconductor.org/packages/ggtree) [![](https://img.shields.io/badge/devel%20version-1.7.11-green.svg?style=flat)](https://github.com/guangchuangyu/ggtree) [![Bioc](http://www.bioconductor.org/shields/years-in-bioc/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#since) [![](https://img.shields.io/badge/download-14439/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree) [![](https://img.shields.io/badge/download-1385/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree)

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![codecov](https://codecov.io/gh/GuangchuangYu/ggtree/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/ggtree) [![Last-changedate](https://img.shields.io/badge/last%20change-2017--04--22-green.svg)](https://github.com/GuangchuangYu/ggtree/commits/master) [![GitHub forks](https://img.shields.io/github/forks/GuangchuangYu/ggtree.svg)](https://github.com/GuangchuangYu/ggtree/network) [![GitHub stars](https://img.shields.io/github/stars/GuangchuangYu/ggtree.svg)](https://github.com/GuangchuangYu/ggtree/stargazers) [![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://awesome-r.com/#awesome-r-graphic-displays)

[![platform](http://www.bioconductor.org/shields/availability/devel/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#archives) [![Build Status](http://www.bioconductor.org/shields/build/devel/bioc/ggtree.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/ggtree/) [![Linux/Mac Travis Build Status](https://img.shields.io/travis/GuangchuangYu/ggtree/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/GuangchuangYu/ggtree) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/Guangchuangyu/ggtree/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/ggtree)

The `ggtree` package extending the `ggplot2` package. It based on grammar of graphics and takes all the good parts of `ggplot2`. `ggtree` is designed for not only viewing phylogenetic tree but also displaying annotation data on the tree.

[![Twitter](https://img.shields.io/twitter/url/https/github.com/GuangchuangYu/ggtree.svg?style=social)](https://twitter.com/intent/tweet?hashtags=ggtree&url=http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12628/abstract&screen_name=guangchuangyu)

For details, please visit our project website, <https://guangchuangyu.github.io/ggtree>.

-   [Documentation](https://guangchuangyu.github.io/ggtree/documentation/)
-   [FAQ](https://guangchuangyu.github.io/ggtree/faq/)
-   [Featured Articles](https://guangchuangyu.github.io/ggtree/featuredArticles/)
-   [Feedback](https://guangchuangyu.github.io/ggtree/#feedback)

------------------------------------------------------------------------

Please cite the following article when using `ggtree`:

**G Yu**, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. ***Methods in Ecology and Evolution***. 2017, 8(1):28-36.

[![](https://img.shields.io/badge/doi-10.1111/2041--210X.12628-green.svg?style=flat)](http://dx.doi.org/10.1111/2041-210X.12628) [![](https://img.shields.io/badge/Altmetric-348-green.svg?style=flat)](https://www.altmetric.com/details/10533079)

------------------------------------------------------------------------

### Citation

[![citation](https://img.shields.io/badge/cited%20by-16-green.svg?style=flat)](https://scholar.google.com.hk/scholar?oi=bibs&hl=en&cites=7268358477862164627) [![](https://img.shields.io/badge/cited%20in%20Web%20of%20Science%20Core%20Collection-6-green.svg?style=flat)](http://apps.webofknowledge.com/InboundService.do?mode=FullRecord&customersID=RID&IsProductCode=Yes&product=WOS&Init=Yes&Func=Frame&DestFail=http%3A%2F%2Fwww.webofknowledge.com&action=retrieve&SrcApp=RID&SrcAuth=RID&SID=S1i9ewwKpMcqaWLpgjp&UT=WOS%3A000393305300004)

       +-+---------+---------+---------+---------+---------+---+
    15 +                                                   *   +
       |                                                       |
       |                                                       |
       |                                                       |
    10 +                                                       +
       |                                                       |
       |                                                       |
       |                                                       |
     5 +                                                       +
       |                                                       |
       |                                                       |
       | *                                                     |
       +-+---------+---------+---------+---------+---------+---+
       2016     2016.2    2016.4    2016.6    2016.8     2017   

### Download stats

[![download](http://www.bioconductor.org/shields/downloads/ggtree.svg)](https://bioconductor.org/packages/stats/bioc/ggtree) [![](https://img.shields.io/badge/download-14439/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree) [![](https://img.shields.io/badge/download-1385/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree)

         ++-------------------+------------------+-------------------+------------------+--------------+
    3000 +                                                                                        *    +
         |                                                                                             |
         |                                                                                             |
    2500 +                                                                                             +
         |                                                                                             |
         |                                                                                             |
         |                                                                                             |
    2000 +                                                                                             +
         |                                                                           *     *  *        |
         |                                                                    *   *                    |
    1500 +                                                                                             +
         |                                                                                             |
         |                                                          *                                  |
         |                                                 *            *  *            *              |
    1000 +                                                    *  *                                     +
         |                                    *     *                                                  |
         |                                *      *     *                                               |
         |                             *                                                               |
     500 +                   *      *                                                                  +
         |                *      *                                                                     |
         |             *                                                                               |
       0 +   *  *   *                                                                                  +
         ++-------------------+------------------+-------------------+------------------+--------------+
        2015               2015.5              2016               2016.5              2017
