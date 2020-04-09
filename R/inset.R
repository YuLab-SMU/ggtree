##' add subplots to tree
##'
##' 
##' @title geom_inset
##' @rdname inset
##' @param insets a list of ggplot objects, named by node number
##' @param width width of inset, relative to the range of x-axis
##' @param height height of inset, relative to the range of y-axis
##' @param hjust horizontal adjustment
##' @param vjust vertical adjustment
##' @param x x position, one of 'node' and 'branch'
##' @param reverse_x whether x axis was reversed by scale_x_reverse
##' @param reverse_y whether y axis was reversed by scale_y_reverse
##' @return inset layer
##' @export
##' @author Guangchuang Yu
geom_inset <- function(insets, width = .1, height = .1, hjust = 0, vjust = 0,
                       x = "node", reverse_x = FALSE, reverse_y = FALSE) {
    structure(list(insets = insets, width = width, height = height,
                   hjust = hjust, vjust = vjust, x = x,
                   reverse_x = reverse_x, reverse_y = reverse_y), class = "tree_inset")
}

##' add insets in a tree
##'
##'
##' @title inset
##' @rdname inset
##' @param tree_view tree view
## @inheritParams geom_inset
##' @return tree view with insets
##' @importFrom rvcheck get_fun_from_pkg
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

    width <- width * diff(range(tree_view$data$x))
    height <- height * diff(range(tree_view$data$y))

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





