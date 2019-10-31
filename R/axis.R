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




##' set axis limits (x or y specific by 'by' parameter) of a ggplot object (left hand side)
##' based on the x (align_x function) or y (align_y function) limits of another ggplot object (right hand side).
##' This is useful for using cowplot or patchwork to align ggplot objects.
##'
##'
##' @title align_x
##' @rdname align_axis
##' @param gg ggplot object
##' @param expand_limits amount to expand the limits
##' @param by one of 'x' or 'y'
##' @return ggplot2 object with new limits and expand
##' @importFrom ggplot2 expand_scale
##' @export
##' @author Guangchuang Yu
align_x <- function(gg, expand_limits = expand_scale(0, 0.6), by = 'x') {
    structure(list(gg = gg, expand_limits = expand_limits, by = by, axis = "x"),
              class = "align_axis")
}

##' @rdname align_axis
##' @title align_y
##' @export
align_y <- function(gg, expand_limits = expand_scale(0, 0.6), by = 'y') { 
    structure(list(gg = gg, expand_limits = expand_limits, by = by, axis = "y"),
              class = "align_axis")
}
