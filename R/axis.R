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

##' expand xlim (ylim) by ratio of x (y) range
##'
##'
##' @rdname ggexpand
##' @param ratio expand x (y) limits by amount of xrange (yrange) * ratio
##' @param direction expand x limit at right hand side if direction is 1, or left hand side if direction is -1
##' @param side one of 'h' for horizontal and 'v' for vertical or 'hv' for both.
##' @return ggexpand object
##' @export
##' @author Guangchuang Yu
ggexpand <- function(ratio, direction = 1, side = 'hv') {
    side <- match.arg(side, c('h', 'v', 'hv'))

    structure(list(ratio = ratio, direction = direction, side = side),
              class = "ggexpand")
}


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
    mx <- max(x, na.rm=TRUE)
    treeview$data$x <- x - mx
    treeview$data$branch <- treeview$data$branch - mx
    treeview
}


ggrange2 <- function(plot, var) {
    ## aplot::ggrange extract panel range
    ## this function extract plot range

    var <- paste0("panel_scales_", var)
    ggplot_build(plot)$layout[[var]][[1]]$range$range
}

