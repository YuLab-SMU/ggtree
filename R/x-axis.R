##' set x axis limits for Tree panel
##'
##'
##' @title xlim_tree
##' @param xlim xlim
##' @return updated tree view
##' @export
##' @author Guangchuang Yu
xlim_tree <- function(xlim) {
    xlim_expand(xlim, panel='Tree')
}


##' expand x axis limits for specific panel
##'
##'
##' @title xlim_expand
##' @param xlim xlim
##' @param panel panel
##' @return updated tree view
##' @importFrom ggplot2 geom_blank
##' @export
##' @author Guangchuang Yu
xlim_expand <- function(xlim, panel) {
    structure(list(x = xlim, panel = panel), class = "facet_xlim")
}




##' Set y axis limits of a ggplot based on the y limits of a ggtree.
##' This is useful for using cowplot or patchwork to properly align ggtree with other ggplot objects.
##'
##'
##' @title ylim_ggtree
##' @param ggtree ggtree object
##' @param expand_limits amount to expand the limits
##' @return ggplot2 object with new y limits
##' @importFrom ggplot2 expand_scale
##' @export
##' @author Guangchuang Yu
ylim_ggtree <- function(ggtree, expand_limits = expand_scale(0, 0.6)) {
    structure(list(ggtree = ggtree, expand_limits = expand_limits),
              class = "ylim_ggtree")
}

##' add second x-axis for geom_range
##'
##'
##' @title scale_x_range
##' @return ggtree object
##' @export
##' @author Guangchuang Yu
scale_x_range <- function() {
    structure(list(), class = "range_xaxis")
}


##' reverse timescle x-axis
##'
##'
##' @title revts
##' @param treeview treeview
##' @return updated treeview
##' @export
##' @author Guangchuang Yu
revts <- function(treeview) {
    x <- treeview$data$x
    mx <- max(x)
    treeview$data$x <- x - mx
    treeview$data$branch <- treeview$data$branch - mx
    treeview
}
