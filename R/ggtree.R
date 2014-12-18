##' drawing phylogenetic tree from phylo object
##'
##' 
##' @title ggtree
##' @param tr phylo object
##' @param showDistance add distance legend, logical
##' @param ... additional parameter
##' @return tree
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 annotate
##' @export
##' @author Yu Guangchuang
ggtree <- function(tr, showDistance=FALSE, ...) {
    d <- x <- y <- NULL
    p <- ggplot(tr, aes(x, y), ...) + geom_tree(...) + xlab("") + ylab("") + theme_tree2()
    if (showDistance == FALSE) {
        p <- p + theme_tree()
    }
    return(p)
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
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @export
##' @author Yu Guangchuang
geom_tiplab <- function(align=FALSE, ...) {
    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        geom_text(aes(x=max(x), label=label), subset=.(isTip), ...)
    } else {
        geom_text(aes(label=label), subset=.(isTip), ...)
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

##' add placement based on edge
##'
##' 
##' @title geom_eplace
##' @param data placement data.frame
##' @param map edge column name
##' @param place place info
##' @param ... additional parameter
##' @return text layer
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 aes
##' @export
##' @author ygc
geom_eplace <- function(data, map, place, ...) {
    edge <- NULL
    ii <- 1:nrow(data)
    ## if (align == TRUE) 
    ##     geom_text(aes(x=max(x)), subset=.(edge %in% data[[edgeCol]]), label = data[[annoCol]], ...)
    geom_text(subset=.(edge %IN% data[[map]]), label = data[ii, place], ...)
    
}

##' add placement based on node
##'
##' 
##' @title geom_nplace
##' @param data placement data.frame
##' @param map edge column name
##' @param place place info
##' @param ... additional parameter
##' @return text layer
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 aes
##' @export
##' @author ygc
geom_nplace <- function(data, map, place, ...) {
    node <- NULL
    ii <- 1:nrow(data)
    geom_text(subset=.(label %IN% data[[map]]), label = data[ii, place], ...)
}

##' place annotation in tree
##'
##' 
##' @title geom_place 
##' @param data data
##' @param map mapping variable
##' @param place placement info
##' @param by one of "node" and "edge"
##' @param ... additional parameter
##' @return text layer
##' @export
##' @author ygc
geom_place <- function(data, map, place, by="node", ...) {
    data <- data[order(data[[map]]),]
    if (by == "node") {
        geom_nplace(data, map, place, ...)
    } else if (by == "edge") {
        geom_eplace(data, map, place, ...)
    } else {
        stop("not supported yet...")
    }
}




##' tree theme
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
    theme_tree2() %+replace%
    theme(axis.line.x = element_line(color="white"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
          )
}

##' tree2 theme
##'
##' 
##' @title theme_tree2
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_line
##' @importFrom ggplot2 %+replace%
##' @export
##' @return NULL 
##' @author Yu Guangchuang
theme_tree2 <- function() {
    theme_bw() %+replace%
    theme(legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(color="black"),
          axis.line.y=element_line(color="white"),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank()
          )
}
