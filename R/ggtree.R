##' drawing phylogenetic tree from phylo object
##'
##' 
##' @title ggtree
##' @param tr phylo object
##' @param showDistance add distance legend, logical
##' @param layout one of phylogram, dendrogram, cladogram, fan, radial and unrooted
##' @param yscale y scale
##' @param ladderize logical
##' @param right logical
##' @param branch.length variable for scaling branch 
##' @param ndigits number of digits to round numerical annotation variable
##' @param ... additional parameter
##' @return tree
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 annotate
##' @importFrom ggplot2 scale_x_reverse
##' @importFrom ggplot2 coord_flip
##' @importFrom ggplot2 coord_polar
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr)
ggtree <- function(tr,
                   showDistance=FALSE,
                   layout="phylogram",
                   yscale="none",
                   ladderize = TRUE, right=FALSE,
                   branch.length="branch.length",
                   ndigits = NULL, ...) {
    d <- x <- y <- NULL
    if (layout == "fan") {
        ## layout <- "phylogram"
        type <- "fan"
    } else if (layout == "radial") {
        layout <- "cladogram"
        type <- "radial"
    } else if (layout == "dendrogram") {
        layout <- "phylogram"
        type <- "dendrogram"
    } else {
        type <- "none"
    }
    p <- ggplot(tr, aes(x, y),
                layout        = layout,
                yscale        = yscale,
                ladderize     = ladderize,
                right         = right,
                branch.length = branch.length,
                ndigits       = ndigits, ...)

    p <- p + geom_tree(layout, ...) + xlab("") + ylab("") + theme_tree2()
    
    if (type == "dendrogram") {
        p <- p + scale_x_reverse() + coord_flip()
    } else if (type == "fan" || type == "radial") {
        p <- p + coord_polar(theta = "y")
    } 
    
    if (showDistance == FALSE) {
        p <- p + theme_tree()
    }
    attr(p, "param") <- list(layout        = layout,
                             yscale        = yscale,
                             ladderize     = ladderize,
                             right         = right,
                             branch.length = branch.length,
                             ndigits       = ndigits)
    return(p)
}

##' add tree layer
##'
##' 
##' @title geom_tree
##' @param layout one of phylogram, cladogram
##' @param color color
##' @param linetype line type
##' @param size line size
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' require(ggplot2)
##' ggplot(tr) + geom_tree()
geom_tree <- function(layout="phylogram", color="black", linetype="solid", size=0.5, ...) {
    x <- y <- parent <- NULL
    if (layout == "phylogram" || layout == "fan") {
        if (length(color) != 1) {
            color <- c(color, color)
        }
        if (length(linetype) != 1) {
            linetype <- c(linetype, linetype)
        }
        if (length(size) != 1) {
            size <- c(size, size)
        }
        geom_segment(aes(x    = c(x[parent], x[parent]),
                         xend = c(x,         x[parent]),
                         y    = c(y,         y[parent]),
                         yend = c(y,         y)),
                     color = color,
                     linetype = linetype,
                     size = size, ...)
    } else if (layout == "cladogram" || layout == "unrooted") {
        geom_segment(aes(x    = x[parent],
                         xend = x,
                         y    = y[parent],
                         yend = y),
                     color = color,
                     linetype = linetype,
                     size = size, ...)
    }
}

##' hilight clade with rectangle
##'
##' 
##' @title geom_hilight 
##' @param tree_object supported tree object
##' @param node internal node
##' @param ... additional parameters
##' @return ggplot layer
##' @importFrom ape extract.clade
##' @export
##' @author Guangchuang Yu
geom_hilight <- function(tree_object, node, ...) {
    clade <- extract.clade(get.tree(tree_object), node)
    idx <- groupOTU(tree_object, clade$tip.label)
    dd <- fortify(tree_object, ...)
    x <- dd[idx == 2, "x"]
    y <- dd[idx == 2, "y"]
    annotate("rect", xmin=min(x)-dd[node, "branch.length"]/2,
             xmax=max(x), ymin=min(y)-0.5, ymax=max(y)+0.5, ...)
}


##' add tip label layer
##'
##' 
##' @title geom_tiplab 
##' @param align align tip lab or not, logical
##' @param hjust horizontal adjustment
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab()
geom_tiplab <- function(align=FALSE, hjust=-.25, ...) {
    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        geom_text(aes(x=max(x), label=label), subset=.(isTip), hjust=hjust, ...)
    } else {
        geom_text(aes(label=label), subset=.(isTip), hjust=hjust, ...)
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
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab(align=TRUE) + geom_aline()
geom_aline <- function(linetype="dashed", ...) {
    x <- y <- isTip <- NULL
    geom_segment(aes(x=ifelse(x==max(x), x, x*1.02),
                     xend=max(x), yend=y),
                 subset=.(isTip), linetype=linetype, ...)
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
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tippoint()
geom_tippoint <- function(...) {
    isTip <- NULL
    geom_point(subset=.(isTip), ...)
}


##' tree theme
##'
##' 
##' @title theme_tree
##' @param bgcolor background color
##' @param fgcolor foreground color
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 %+replace%
##' @export
##' @return updated ggplot object with new theme
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree()
theme_tree <- function(bgcolor="white", fgcolor="black") {
    theme_tree2() %+replace%
    theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
          axis.line.x = element_line(color=bgcolor),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
          )
}

##' tree2 theme
##'
##' 
##' @title theme_tree2
##' @param bgcolor background color
##' @param fgcolor foreground color
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_line
##' @importFrom ggplot2 %+replace%
##' @importFrom ggplot2 element_rect
##' @export
##' @return updated ggplot object with new theme
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree2()
theme_tree2 <- function(bgcolor="white", fgcolor="black") {
    theme_bw() %+replace%
    theme(legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background=element_rect(fill=bgcolor, colour=bgcolor),
          panel.border=element_blank(),
          axis.line=element_line(color=fgcolor),
          axis.line.y=element_line(color=bgcolor),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank()
          )
}

##' hilight clade with rectangle
##'
##' 
##' @title hilight
##' @param tree_view tree view 
##' @param node clade node
##' @param fill fill color
##' @param alpha alpha
##' @param ... additional parameter
##' @return tree view
##' @export
##' @author Guangchuang Yu
hilight <- function(tree_view, node, fill="steelblue", alpha=0.5, ...) {
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    sp.df <- df[c(sp, node),]
    x <- sp.df$x
    y <- sp.df$y
    tree_view + annotate("rect", xmin=min(x)-df[node, "branch.length"]/2,
                         xmax=max(x), ymin=min(y)-0.5, ymax=max(y)+0.5,
                         fill = fill, alpha = alpha, ...)
}

##' collapse a clade
##'
##' 
##' @title collapse_clade
##' @param tree_view tree view 
##' @param node clade node
##' @return tree view
##' @export
##' @author Guangchuang Yu
collapse_clade <- function(tree_view, node) {
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    sp.df <- df[sp,]
    df[node, "isTip"] <- TRUE
    sp_y <- range(sp.df$y)
    ii <- which(df$y > max(sp_y))
    if (length(ii)) {
        df$y[ii] <- df$y[ii] - diff(sp_y)
    }
    df$y[node] <- min(sp_y)

    df[sp, "x"] <- NA
    df[sp, "y"] <- NA

    root <- which(df$node == df$parent)
    pp <- df[node, "parent"]
    while(any(pp != root)) {
        df[pp, "y"] <- mean(df[getChild.df(df, pp), "y"])
        pp <- df[pp, "parent"]
    }
    j <- getChild.df(df, pp)
    j <- j[j!=pp]
    df[pp, "y"] <- mean(df[j, "y"])
    
    tree_view$data <- df
    clade <- paste0("clade_", node)
    attr(tree_view, clade) <- sp.df
    tree_view
}

##' expand collased clade
##'
##' 
##' @title expand_clade
##' @param tree_view tree view
##' @param node clade node
##' @return tree view
##' @export
##' @author Guangchuang Yu
expand_clade <- function(tree_view, node) {
    clade <- paste0("clade_", node)
    sp.df <- attr(tree_view, clade)
    if (is.null(sp.df)) {
        return(tree_view)
    }
    df <- tree_view$data
    df[node, "isTip"] <- FALSE
    sp_y <- range(sp.df$y)
    ii <- which(df$y > df$y[node])
    df[ii, "y"] <- df[ii, "y"] + diff(sp_y)
    df[sp.df$node,] <- sp.df

    root <- which(df$node == df$parent)
    pp <- node
    while(any(pp != root)) {
        df[pp, "y"] <- mean(df[getChild.df(df, pp), "y"])
        pp <- df[pp, "parent"]
    }
    j <- getChild.df(df, pp)
    j <- j[j!=pp]
    df[pp, "y"] <- mean(df[j, "y"])
    
    tree_view$data <- df
    attr(tree_view, clade) <- NULL
    tree_view
}
