##' add insets in a tree
##'
##' 
##' @title inset
##' @param tree_view tree view
##' @param insets a list of ggplot objects, named by node number
##' @param width width of inset
##' @param height height of inset
##' @param hjust horizontal adjustment
##' @param vjust vertical adjustment
##' @param x x position, one of 'node' and 'branch'
##' @return tree view with insets
##' @export
##' @author Guangchuang Yu
inset <- function(tree_view, insets, width=0.1, height=0.1, hjust=0, vjust=0, x="node") {
    df <- tree_view$data[as.numeric(names(insets)),]
    x <- match.arg(x, c("node", "branch", "edge"))

    if (x == 'node') {
        xx <- df$x
    } else {
        xx <- df$branch
    }
    yy <- df$y
    
    xx <- xx - hjust
    yy <- yy - vjust

    for (i in seq_along(insets)) {
        tree_view %<>% subview(insets[[i]],
                               x = xx[i],
                               y = yy[i],
                               width = width,
                               height = height)
    }
    return(tree_view)
}

##' generate a list of bar charts for results of ancestral state reconstruction
##'
##' 
##' @title nodebar
##' @param position position of bar, one of 'stack' and 'dodge'
##' @inheritParams nodepie
##' @return list of ggplot objects
##' @export
##' @importFrom ggplot2 geom_bar
##' @importFrom tidyr gather
##' @author Guangchuang Yu
nodebar <- function(data, cols, color, alpha=1, position="stack") {
    if (! "node" %in% colnames(data)) {
        stop("data should have a column 'node'...")
    }
    type <- value <- NULL
    
    ldf <- gather(data, type, value, cols) %>% split(., .$node)
    bars <- lapply(ldf, function(df) ggplot(df, aes_(x=1, y=~value, fill=~type)) +
                                     geom_bar(stat='identity', alpha=alpha, position=position) +
                                     theme_inset()
                   )

    if (missingArg(color) || is.null(color) || is.na(color)) {
        ## do nothing
    } else {
        bars <- lapply(bars, function(p) p+scale_fill_manual(values=color))
    }
    return(bars)
}

##' generate a list of pie charts for results of ancestral stat reconstruction
##'
##' 
##' @title nodepie
##' @param data a data.frame of stats with an additional column of node number
##' @param cols column of stats
##' @param color color of bar
##' @param alpha alpha
##' @return list of ggplot objects
##' @export
##' @author Guangchuang Yu
nodepie <- function(data, cols, color, alpha=1) {
    if (! "node" %in% colnames(data)) {
        stop("data should have a column 'node'...")
    }
    type <- value <- NULL
    if (missingArg(color)) {
        color <- NA
    }
    ldf <- gather(data, type, value, cols) %>% split(., .$node)
    lapply(ldf, function(df) ggpie(df, y=~value, fill=~type, color, alpha))
}


ggpie <- function(data, y, fill, color, alpha=1) {
    p <- ggplot(data, aes_(x=1, y=y, fill=fill)) +
        geom_bar(stat='identity', alpha=alpha) +
        coord_polar(theta='y') + theme_inset()
    
    if (missingArg(color) || is.null(color) || is.na(color)) {
        ## do nothing
    } else {
        p <- p+scale_fill_manual(values=color)
    }
    return(p)
}





