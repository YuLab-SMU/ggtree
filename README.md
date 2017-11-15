<!-- README.md is generated from README.Rmd. Please edit that file -->
ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data
===========================================================================================================================

<img src="https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/ggtree/ggtree.png" height="200" align="right" />

[![releaseVersion](https://img.shields.io/badge/release%20version-1.10.0-green.svg?style=flat)](https://bioconductor.org/packages/ggtree)
[![develVersion](https://img.shields.io/badge/devel%20version-1.11.1-green.svg?style=flat)](https://github.com/guangchuangyu/ggtree)
[![Bioc](http://www.bioconductor.org/shields/years-in-bioc/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#since)
[![total](https://img.shields.io/badge/downloads-21262/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree)
[![month](https://img.shields.io/badge/downloads-1156/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree)

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/GuangchuangYu/ggtree/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/ggtree)
[![Last-changedate](https://img.shields.io/badge/last%20change-2017--11--15-green.svg)](https://github.com/GuangchuangYu/ggtree/commits/master)
[![GitHub
forks](https://img.shields.io/github/forks/GuangchuangYu/ggtree.svg)](https://github.com/GuangchuangYu/ggtree/network)
[![GitHub
stars](https://img.shields.io/github/stars/GuangchuangYu/ggtree.svg)](https://github.com/GuangchuangYu/ggtree/stargazers)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://awesome-r.com/#awesome-r-graphic-displays)

[![platform](http://www.bioconductor.org/shields/availability/devel/ggtree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/ggtree.html#archives)
[![Build
Status](http://www.bioconductor.org/shields/build/devel/bioc/ggtree.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/ggtree/)
[![Linux/Mac Travis Build
Status](https://img.shields.io/travis/GuangchuangYu/ggtree/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/GuangchuangYu/ggtree)
[![AppVeyor Build
Status](https://img.shields.io/appveyor/ci/Guangchuangyu/ggtree/master.svg?label=Windows)](https://ci.appveyor.com/project/GuangchuangYu/ggtree)
[![Backers on Open
Collective](https://opencollective.com/ggtree/backers/badge.svg)](#backers)
[![Sponsors on Open
Collective](https://opencollective.com/ggtree/sponsors/badge.svg)](#sponsors)

The `ggtree` package extending the `ggplot2` package. It based on
grammar of graphics and takes all the good parts of `ggplot2`. `ggtree`
is designed for not only viewing phylogenetic tree but also displaying
annotation data on the tree.

[![Twitter](https://img.shields.io/twitter/url/https/github.com/GuangchuangYu/ggtree.svg?style=social)](https://twitter.com/intent/tweet?hashtags=ggtree&url=http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12628/abstract&screen_name=guangchuangyu)

For details, please visit our project website,
<https://guangchuangyu.github.io/ggtree>.

-   [Documentation](https://guangchuangyu.github.io/ggtree/documentation/)
-   [FAQ](https://guangchuangyu.github.io/ggtree/faq/)
-   [Featured
    Articles](https://guangchuangyu.github.io/ggtree/featuredArticles/)
-   [Feedback](https://guangchuangyu.github.io/ggtree/#feedback)

------------------------------------------------------------------------

Please cite the following article when using `ggtree`:

**G Yu**, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an R
package for visualization and annotation of phylogenetic trees with
their covariates and other associated data. ***Methods in Ecology and
Evolution***. 2017, 8(1):28-36.

[![doi](https://img.shields.io/badge/doi-10.1111/2041--210X.12628-green.svg?style=flat)](http://dx.doi.org/10.1111/2041-210X.12628)
[![Altmetric](https://img.shields.io/badge/Altmetric-336-green.svg?style=flat)](https://www.altmetric.com/details/10533079)
[![citation](https://img.shields.io/badge/cited%20by-47-green.svg?style=flat)](https://scholar.google.com.hk/scholar?oi=bibs&hl=en&cites=7268358477862164627)

------------------------------------------------------------------------

### Citation

<img src="docs/images/citation.png" width="890"/>

### Download stats

[![download](http://www.bioconductor.org/shields/downloads/ggtree.svg)](https://bioconductor.org/packages/stats/bioc/ggtree)
[![total](https://img.shields.io/badge/downloads-21262/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree)
[![month](https://img.shields.io/badge/downloads-1156/month-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/ggtree)

<img src="docs/images/dlstats.png" width="890"/>

Contributors
------------

This project exists thanks to all the people who contribute.
[\[Contribute\]](CONTRIBUTING.md).
<a href="https://github.com/GuangchuangYu/ggtree/graphs/contributors"><img src="https://opencollective.com/ggtree/contributors.svg?width=890" /></a>

Backers
-------

Thank you to all our backers! 🙏 \[[Become a
backer](https://opencollective.com/ggtree#backer)\]

<a href="https://opencollective.com/ggtree#backers" target="_blank"><img src="https://opencollective.com/ggtree/backers.svg?width=890"></a>

Sponsors
--------

Support this project by becoming a sponsor. Your logo will show up here
with a link to your website. \[[Become a
sponsor](https://opencollective.com/ggtree#sponsor)\]

<a href="https://opencollective.com/ggtree/sponsor/0/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/0/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/1/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/1/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/2/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/2/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/3/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/3/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/4/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/4/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/5/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/5/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/6/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/6/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/7/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/7/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/8/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/8/avatar.svg"></a>
<a href="https://opencollective.com/ggtree/sponsor/9/website" target="_blank"><img src="https://opencollective.com/ggtree/sponsor/9/avatar.svg"></a>
