##' gemo_inset can add subplots to tree by accepting a list of ggplot objects that are ancestral 
##' stats or data associated with selected nodes in the tree. These ggplot objects can be any 
##' kind of charts or hybrid of of these charts.
##' 
##' Users can also use 
##' 
##' 
##' @title geom_inset
##' @rdname inset
##' @param insets a list of ggplot objects, named by node number
##' @param width width of the inset, relative to the range of x-axis, defaults to .1
##' @param height height of the inset, relative to the range of y-axis, defaults to .1
##' @param hjust adjust the horizontal position of the charts, charts will go left if hjust > 0
##' @param vjust adjust the vertical position of the charts, charts will go down if vjust > 0
##' @param x the position where users want to place the charts, one of 'node' (default) and 'branch'
##' @param reverse_x whether to reverse x axis of the charts by 'ggplot2::scale_x_reverse', defaults to 'FALSE'
##' @param reverse_y whether to reverse y axis of the charts by 'ggplot2::scale_y_reverse', defaults to 'FALSE'
##' @return inset layer
##' @export
##' @author Guangchuang Yu
##' @references
##' For demonstration of this function, please refer to chapter 8.3 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_inset <- function(insets, width = .1, height = .1, hjust = 0, vjust = 0,
                       x = "node", reverse_x = FALSE, reverse_y = FALSE) {
    structure(list(insets = insets, width = width, height = height,
                   hjust = hjust, vjust = vjust, x = x,
                   reverse_x = reverse_x, reverse_y = reverse_y), class = "tree_inset")
}

##' add subplots as insets in a tree
##'
##'
##' @title inset
##' @rdname inset
##' @param tree_view tree view 
##' @return tree view with insets
##' @importFrom yulab.utils get_fun_from_pkg
##' @export
##' @author Guangchuang Yu
inset <- function(tree_view, insets, width, height, hjust=0, vjust=0,
                  x="node", reverse_x=FALSE, reverse_y=FALSE) {

    if(width < 0 || width > 1)
        stop("width should be in range of (0,1)")

    if(height < 0 || height > 1)
        stop("height should be in range of (0,1)")

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
    if (reverse_x)
        xx <- -xx
    if (reverse_y)
        yy <- -yy

    width <- width * diff(range(tree_view$data$x, na.rm = TRUE))
    height <- height * diff(range(tree_view$data$y, na.rm = TRUE))

    geom_subview <- get_fun_from_pkg("ggimage", "geom_subview")

    tree_view + geom_subview(subview = insets,
                            width = width,
                            height = height,
                            x = xx,
                            y = yy)
}

##' generate a list of bar charts for results of ancestral state reconstruction
##'
##'
##' @title nodebar
##' @param position position of bars, if 'stack' (default) make bars stacked atop one another, 'dodge' make them dodged side-to-side
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

    ldf <- gather(data, type, value, !! cols) %>% split(., .$node)
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
##' @param data a data.frame of stats with an additional column of node number named "node"
##' @param cols columns of the data.frame that store the stats
##' @param color set color of bars
##' @param alpha set transparency of the charts
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
    ldf <- gather(data, type, value, !! cols) %>% split(., .$node)
    lapply(ldf, function(df) ggpie(df, y=~value, fill=~type, color, alpha))
}


##' @importFrom methods missingArg
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

