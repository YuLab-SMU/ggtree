##' @title hexpand
##' @rdname ggexpand
##' @export
hexpand <- function(ratio, direction = 1) {
    ggexpand(ratio, direction, side = 'h')
}

##' @title vexpand
##' @rdname ggexpand
##' @export
vexpand <- function(ratio, direction = 1) {
    ggexpand(ratio, direction, side = 'v')
}

##' expand xlim (ylim) by ratio of x (y) axis range
##'
##'
##' @rdname ggexpand
##' @param ratio expand x (y) axis limits by amount of xrange (yrange) * ratio
##' @param direction expand x axis limit at right hand side if direction is 1 (default), or left hand side if direction is -1
##' @param side one of 'h' for horizontal and 'v' for vertical or 'hv' for both (default).
##' @return ggexpand object
##' @export
##' @examples
##' x <- rtree(20)
##' x$tip.label <- paste0('RRRRREEEEEAAAAALLLLLYYYYY_Long_Lable_', x$tip.label)
##' p1 <- ggtree(x) + geom_tiplab()
##' p1 + ggexpand(1.5, side = "h")
##' @author Guangchuang Yu
ggexpand <- function(ratio, direction = 1, side = 'hv') {
    side <- match.arg(side, c('h', 'v', 'hv'))

    structure(list(ratio = ratio, direction = direction, side = side),
              class = "ggexpand")
}


##' set x axis limits specially for Tree panel
##'
##'
##' @title xlim_tree
##' @param xlim x axis limits 
##' @return updated tree view
##' @export
##' @examples
##' x <- rtree(30)
##' p <- ggtree(x) + geom_tiplab()
##' d <- data.frame(label = x$tip.label, 
##'                 value = rnorm(30))
##' p2 <- p + geom_facet(panel = "Dot", data = d, 
##'             geom = geom_point, mapping = aes(x = value))
##' p2 + xlim_tree(6)
##' @author Guangchuang Yu
xlim_tree <- function(xlim) {
    xlim_expand(xlim, panel='Tree')
}


##' expand x axis limits for specific panel
##'
##'
##' @title xlim_expand
##' @param xlim x axis limits
##' @param panel name of the panel to expand
##' @return updated tree view
##' @importFrom ggplot2 geom_blank
##' @export
##' @examples
##' x <- rtree(30)
##' p <- ggtree(x) + geom_tiplab()
##' d <- data.frame(label = x$tip.label, 
##'                 value = rnorm(30))
##' p2 <- p + geom_facet(panel = "Dot", data = d, 
##'             geom = geom_point, mapping = aes(x = value))
##' p2 + xlim_expand(c(-10, 10), 'Dot')
##' @author Guangchuang Yu
xlim_expand <- function(xlim, panel) {
    structure(list(x = xlim, panel = panel), class = "facet_xlim")
}



##' add second x-axis for geom_range
##'
##' notice that the first axis is disabled in the default theme thus users need to enable it first before using scale_x_range
##'
##' @title scale_x_range
##' @return ggtree object
##' @export
##' @references
##' For demonstration of this function ,please refer to chapter 5.2.4 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
##' @author Guangchuang Yu
scale_x_range <- function() {
    structure(list(), class = "range_xaxis")
}


##' reverse timescle x-axis by setting the most recent tip to 0
##'
##' 'scale_x_continuous(labels=abs)' is required if users want to set the x-axis lable to absolute value
##'
##'
##' @title revts
##' @param treeview original tree view
##' @return updated tree view
##' @export
##' @examples
##' tr <- rtree(10)
##' p <- ggtree(tr) + theme_tree2()
##' p2 <- revts(p) 
##' p2 + scale_x_continuous(labels=abs)
##' @author Guangchuang Yu
revts <- function(treeview) {
    x <- treeview$data$x
    mx <- max(x, na.rm=TRUE)
    treeview$data$x <- x - mx
    treeview$data$branch <- treeview$data$branch - mx
	tip.edge.len <- attr(treeview$data, 'tip.edge.len')
    if (!is.null(tip.edge.len)){
        treeview$data[treeview$data$isTip,"x", drop=TRUE] <- tip.edge.len
    }
    treeview
}

