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
    dummy <- data.frame(x=xlim, .panel=panel)
    geom_blank(aes_(x=~x), dummy, inherit.aes = FALSE)
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
    x <- x - max(x)
    treeview$data$x <- x
    treeview
}
