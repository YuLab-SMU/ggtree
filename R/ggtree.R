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
    lineend  = "round"
    if (layout == "phylogram" || layout == "fan") {
        if (length(color) != 1) {
            color <- rep(color, 2) ## c(color, color)
        }
        if (length(linetype) != 1) {
            linetype <- rep(linetype, 2) ## c(linetype, linetype)
        }
        if (length(size) != 1) {
            size <- rep(size, 2) ## c(size, size)
        }
        geom_segment(aes(x    = c(x[parent], x[parent]),
                         xend = c(x,         x[parent]),
                         y    = c(y,         y[parent]),
                         yend = c(y,         y)),
                     color    = color,
                     linetype = linetype,
                     size     = size,
                     lineend  = lineend, ...)
    } else if (layout == "cladogram" || layout == "unrooted") {
        geom_segment(aes(x    = x[parent],
                         xend = x,
                         y    = y[parent],
                         yend = y),
                     color    = color,
                     linetype = linetype,
                     size     = size,
                     lineend  = lineend, ...)
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
geom_tiplab <- function(align=FALSE, hjust=0, ...) {
    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        geom_text(aes(x=max(x)+ diff(range(x))/200, label=label), subset=.(isTip), hjust=hjust, ...)
    } else {
        geom_text(aes(x = x + diff(range(x))/200, label=label), subset=.(isTip), hjust=hjust, ...)
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

##' scale clade
##'
##' 
##' @title scaleClade
##' @param tree_view tree view
##' @param node clade node
##' @param scale scale
##' @param vertical_only logical. If TRUE, only vertical will be scaled.
##' If FALSE, the clade will be scaled vertical and horizontally.
##' TRUE by default.
##' @return tree view
##' @export
##' @author Guangchuang Yu
scaleClade <- function(tree_view, node, scale=1, vertical_only=TRUE) {
    if (scale == 1) {
        return(tree_view)
    }
    
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    sp.df <- df[sp,]
    
    ## sp_nr <- nrow(sp.df)
    ## span <- diff(range(sp.df$y))/sp_nr
    
    ## new_span <- span * scale
    old.sp.df <- sp.df
    sp.df$y <- df[node, "y"] + (sp.df$y - df[node, "y"]) * scale
    if (vertical_only == FALSE) {
        sp.df$x <- df[node, "x"] + (sp.df$x - df[node, "x"]) * scale
    }
    
    scale_diff.up <- max(sp.df$y) - max(old.sp.df$y)
    scale_diff.lw <- min(sp.df$y) - min(old.sp.df$y)
    
    ii <- df$y > max(old.sp.df$y)
    if (sum(ii) > 0) {
        df[ii, "y"] <- df[ii, "y"] + scale_diff.up
    }
    
    jj <- df$y < min(old.sp.df$y)
    if (sum(jj) > 0) {
        df[jj, "y"] <- df[jj, "y"] + scale_diff.lw
    }
    
    df[sp,] <- sp.df
    
    if (! "scale" %in% colnames(df)) {
        df$scale <- 1
    }
    df[sp, "scale"] <- df[sp, "scale"] * scale

    ## re-calculate branch mid position
    df <- calculate_branch_mid(df)
    
    tree_view$data <- df
    tree_view
}


##' flip position of two selected branches
##'
##' 
##' @title flip
##' @param tree_view tree view 
##' @param node1 node number of branch 1
##' @param node2 node number of branch 2
##' @return ggplot2 object
##' @export
##' @author Guangchuang Yu
flip <- function(tree_view, node1, node2) {
    df <- tree_view$data
    p1 <- with(df, parent[node == node1])
    p2 <- with(df, parent[node == node2])

    if (p1 != p2) {
        stop("node1 and node2 should share a same parent node...")
    }

    sp1 <- c(node1, get.offspring.df(df, node1))
    sp2 <- c(node2, get.offspring.df(df, node2))

    sp1.df <- df[sp1,]
    sp2.df <- df[sp2,]

    min_y1 <- min(sp1.df$y)
    min_y2 <- min(sp2.df$y)

    if (min_y1 < min_y2) {
        tmp <- sp1.df
        sp1.df <- sp2.df
        sp2.df <- tmp
        tmp <- sp1
        sp1 <- sp2
        sp2 <- tmp
    }

    min_y1 <- min(sp1.df$y)
    min_y2 <- min(sp2.df$y)

    space <- min(sp1.df$y) - max(sp2.df$y)
    sp1.df$y <- sp1.df$y - abs(min_y1 - min_y2)
    sp2.df$y <- sp2.df$y + max(sp1.df$y) + space - min(sp2.df$y)

    df[sp1, "y"] <- sp1.df$y
    df[sp2, "y"] <- sp2.df$y

    anc <- getAncestor.df(df, node1)
    ii <- match(anc, df$node)
    df[ii, "y"] <- NA
    currentNode <- as.vector(sapply(anc, getChild.df, df=df))
    currentNode <- currentNode[!currentNode %in% anc]
    
    tree_view$data <- re_assign_ycoord_df(df, currentNode)
    tree_view
}

##' rotate 180 degree of a selected branch
##'
##' 
##' @title rotate
##' @param tree_view tree view 
##' @param node selected node
##' @return ggplot2 object
##' @export
##' @author Guangchuang Yu
rotate <- function(tree_view, node) {
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    sp_idx <- with(df, match(sp, node))
    tip <- sp[df$isTip[sp_idx]]
    sp.df <- df[sp_idx,]
    ii <- with(sp.df, match(tip, node))
    jj <- ii[order(sp.df[ii, "y"])]
    sp.df[jj,"y"] <- rev(sp.df[jj, "y"])
    sp.df[-jj, "y"] <- NA
    sp.df <- re_assign_ycoord_df(sp.df, tip)

    df[sp_idx, "y"] <- sp.df$y
    df[df$node == node, "y"] <- mean(df[df$parent == node, "y"])
    pnode <- df$parent[df$node == node]
    if (pnode != node && !is.na(pnode)) {
        df[df$node == pnode, "y"] <- mean(df[df$parent == pnode, "y"])
    }
    tree_view$data <- df
    tree_view
}

re_assign_ycoord_df <- function(df, currentNode) {
    while(any(is.na(df$y))) {
        pNode <- with(df, parent[match(currentNode, node)]) %>% unique
        idx <- sapply(pNode, function(i) with(df, all(node[parent == i & parent != node] %in% currentNode)))
        newNode <- pNode[idx]
        ## newNode <- newNode[is.na(df[match(newNode, df$node), "y"])]
        
        df[match(newNode, df$node), "y"] <- sapply(newNode, function(i) {
            with(df, mean(y[parent == i], na.rm = TRUE))
        })
        traced_node <- as.vector(sapply(newNode, function(i) with(df, node[parent == i])))
        currentNode <- c(currentNode[! currentNode %in% traced_node], newNode)
    }
    return(df)
}

##' collapse a clade
##'
##' 
##' @title collapse
##' @param tree_view tree view 
##' @param node clade node
##' @return tree view
##' @export
##' @seealso expand
##' @author Guangchuang Yu
collapse <- function(tree_view, node) {
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

    ## re-calculate branch mid position
    df <- calculate_branch_mid(df)
    
    tree_view$data <- df
    clade <- paste0("clade_", node)
    attr(tree_view, clade) <- sp.df
    tree_view
}

##' expand collased clade
##'
##' 
##' @title expand
##' @param tree_view tree view
##' @param node clade node
##' @return tree view
##' @export
##' @seealso collapse
##' @author Guangchuang Yu
expand <- function(tree_view, node) {
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
    
    sp.df$y <- sp.df$y - min(sp.df$y) + df$y[node]
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

    ## re-calculate branch mid position
    df <- calculate_branch_mid(df)
    
    tree_view$data <- df
    attr(tree_view, clade) <- NULL
    tree_view
}

##' add colorbar legend
##'
##' 
##' @title add_colorbar
##' @param p tree view
##' @param color output of scale_color function
##' @param x x position
##' @param ymin ymin
##' @param ymax ymax
##' @param font.size font size 
##' @return ggplot2 object
##' @export
##' @importFrom ggplot2 annotate
##' @author Guangchuang Yu
add_colorbar <- function(p, color, x=NULL, ymin=NULL, ymax=NULL, font.size=4) {
    legend <- do.call("cbind", attr(color, "scale"))
    
    legend[,1] <- round(as.numeric(legend[,1]), 2)
    
    ## legend[nrow(legend),1] <- paste(">=", legend[nrow(legend),1])

    if (is.null(x)) {
        xx <- range(p$data$x)
        x <- min(xx)+diff(xx)/100
    }

    yy <- range(p$data$y)
    if (is.null(ymin)) {
        if (is.null(ymax)) {        
            ymax <- max(yy) - diff(yy)/100
        }
        ymin <- ymax - diff(yy)/15
    }

    if (is.null(ymax)) {
        ymax <- ymin + diff(yy)/15
    }
        
    yy <- seq(ymin, ymax, length.out=nrow(legend)+1)

    ymin <- yy[1:nrow(legend)]
    ymax <- yy[2:length(yy)]
    y <- (ymin+ymax)/2

    i <- seq(1, length(y), length.out = 5) %>% round(0)
    offset <- diff(range(p$data$x))/40
    barwidth <- offset/5
    
    p + annotate("text", x=x+offset*1.5, y=y[i], label=legend[i,1], size=font.size, hjust=0) +
        annotate("rect", xmin=x, xmax=x+offset, ymin=ymin,
                 ymax = ymax, fill=legend[,2], color=legend[,2]) +
                     annotate("segment", x=x, xend=x+barwidth, y=y[i], yend=y[i], color="white") +
                         annotate("segment", x=x+offset-barwidth, xend=x+offset, y=y[i], yend=y[i], color="white")
    
}

##' add evolution distance legend
##'
##' 
##' @title add_legend
##' @param p tree view
##' @param x x position
##' @param y y position
##' @param offset offset of text and line
##' @param font.size font size
##' @param ... additional parameter
##' @return tree view
##' @export
##' @author Guangchuang Yu
add_legend <- function(p, x=NULL, y=NULL, offset=NULL, font.size=4, ...) {
    if (is.null(x)) {
        x <- min(p$data$x)
    }
    if (is.null(y)) {
        y <- -0.5
    }

    d <- p$data$x %>% range %>% diff
    d <- d/20 
    n <- 0
    while (d < 1) {
        d <- d*10
        n <- n + 1
    }
    d <- floor(d)/(10^n)
    if (is.null(offset)) {
        offset <- p$data$y %>% range %>% diff
        offset <- offset / 100
    }
    p <- p + geom_segment(x=x, y=y, xend=x+d, yend=y, ...) +
        geom_text(x=x+d/2, y=y-offset, label=d, size=font.size, ...) +
            geom_segment(x=x, y=y-offset/2, xend=x, yend=y+offset/2, ...) +
                geom_segment(x=x+d, y=y-offset/2, xend=x+d, yend=y+offset/2, ...)
    return(p)
}

##' get taxa name of a selected node
##'
##' 
##' @title get_taxa_name
##' @param tree_view tree view
##' @param node node
##' @return taxa name vector
##' @export
##' @author Guangchuang Yu
get_taxa_name <- function(tree_view, node) {
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    res <- df[sp, "label"]
    return(res[df[sp, "isTip"]])
}
