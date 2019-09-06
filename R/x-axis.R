##' set x axis limits for Tree panel
##'
##'
##' @title xlim_tree
##' @param xlim xlim
##' @return updated tree view
##' @export
##' @author guangchuang yu
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
##' @author guangchuang yu
xlim_expand <- function(xlim, panel) {
    structure(list(x = xlim, panel = panel), class = "facet_xlim")
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
##' @author guangchuang yu
revts <- function(treeview) {
    x <- treeview$data$x
    mx <- max(x)
    treeview$data$x <- x - mx
    treeview$data$branch <- treeview$data$branch - mx
    treeview
}
