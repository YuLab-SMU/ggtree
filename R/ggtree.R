##' drawing phylogenetic tree from phylo object
##'
##' 
##' @title ggtree
##' @param tr phylo object
##' @param ... additional parameter
##' @return tree
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 annotate
##' @export
##' @author Yu Guangchuang
ggtree <- function(tr, ...) {
    d <- x <- y <- NULL
    ggplot(tr, aes(x, y)) + geom_tree(...) +
        geom_segment(aes(x=0, xend= d <<- round(diff(range(length, na.rm=T))/2,1), y=0, yend=0)) +
            annotate(geom="text", x=d/2, y=-0.6, label=d, size=5) +
                xlab("") + ylab("") + theme_tree()
}

##' add tree layer
##'
##' 
##' @title geom_tree
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
geom_tree <- function(...) {
    x <- y <- parent <- NULL
    geom_segment(aes(x=c(x[parent], x[parent]),
                     xend=c(x, x[parent]),
                     y=c(y, y[parent]),
                     yend=c(y, y)),...) 
}

##' add tip label layer
##'
##' 
##' @title geom_tiplab 
##' @param align align tip lab or not, logical
##' @param hjust horizontal adjustment
##' @param size label size
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @export
##' @author Yu Guangchuang
geom_tiplab <- function(align=FALSE, hjust=-.25, size=5, ...) {
    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        geom_text(aes(x=max(x), label=label), subset=.(isTip), hjust=hjust, size=size, ...)
    } else {
        geom_text(aes(label=label), subset=.(isTip), hjust=hjust, size=size, ...)
    }
}

##' add horizontal align lines
##'
##' 
##' @title geom_aline
##' @param linetype line type
##' @param ... additional parameter
##' @return aline layer
##' @export
##' @author Yu Guangchuang
geom_aline <- function(linetype="dashed", ...) {
    x <- y <- isTip <- NULL
    geom_segment(aes(x=ifelse(x==max(x), x, x*1.02), xend=max(x), yend=y), subset=.(isTip), linetype=linetype, ...)
}

##' add points layer of tips 
##'
##' 
##' @title geom_tippoint 
##' @param ... additional parameter
##' @return tip point layer
##' @importFrom ggplot2 geom_point
##' @export
##' @author Yu Guangchuang
geom_tippoint <- function(...) {
    isTip <- NULL
    geom_point(subset=.(isTip), ...)
}

##' blank theme
##'
##' 
##' @title theme_tree
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 %+replace%
##' @export
##' @return NULL 
##' @author Yu Guangchuang
theme_tree <- function() {
    theme_bw() %+replace%
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank())
}

