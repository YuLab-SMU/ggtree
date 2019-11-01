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




##' set axis limits (x or y) of a `ggplot` object (left hand side of `+`)
##' based on the x (`xlim2`) or y (`ylim2`) limits of another `ggplot` object (right hand side of `+`).
##' This is useful for using `cowplot` or `patchwork` to align `ggplot` objects.
##'
##'
##' @title xlim2
##' @rdname align_axis
##' @param gg ggplot object
##' @param limits vector of limits. If NULL, determine from `gg`. 
##' @return ggplot2 object with new limits
##' @export
##' @author Guangchuang Yu
xlim2 <- function(gg, limits = NULL) {
    axis_align(gg = gg, limits = limits, axis = 'x')
}

##' @rdname align_axis
##' @title ylim2
##' @export
ylim2 <- function(gg, limits = NULL) {
    axis_align(gg = gg, limits = limits, axis = 'y')
}

axis_align <- function(gg, limits = NULL, axis) {
    if (is.null(limits)) {
        if (axis == "x") {
            limits <- xrange(gg)
        } else {
            limits <- yrange(gg)
        }
    }
    structure(list(limits = limits, axis = axis),
              class = "axisAlign")
}


yrange <- function(gg) {
    ggrange(gg, "y")
}

xrange <- function(gg) {
    ggrange(gg, "x")
}

##' @importFrom ggplot2 layer_scales
ggrange <- function(gg, var) {
    res <- layer_scales(gg)[[var]]$range$range
    if (is.character(res)) return(res)

    var <- paste0(var, ".range")
    ggplot_build(gg)$layout$panel_params[[1]][[var]]
}

