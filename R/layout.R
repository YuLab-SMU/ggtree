##' rotate circular tree
##'
##' 
##' @title rotate_tree
##' @param treeview tree view
##' @param angle angle
##' @return updated tree view
##' @export
##' @author Guangchuang Yu
rotate_tree <- function(treeview, angle) {
    treeview <- treeview + coord_polar(theta='y', start=(angle-90)/180*pi, -1)
    treeview$data$angle <- treeview$data$angle + angle
    assign("layout", "circular", envir = treeview$plot_env)
    return(treeview)
}

##' open tree with specific angle
##'
##' 
##' @title open_tree
##' @param treeview tree view
##' @param angle angle
##' @return updated tree view
##' @importFrom ggplot2 scale_y_continuous
##' @export
##' @author Guangchuang Yu
open_tree <- function(treeview, angle) {
    p <- treeview + layout_circular()
    ymax <- max(range(p$data$y))
    p <- p + scale_y_continuous(limits = c(0,
                                           max(c(ymax * (1+angle/(360-angle)), ymax+1))
                                           ))
    N <- nrow(p$data)
    idx <- match(1:N, order(p$data$y))
    NN <- N *(1+angle/(360-angle))
    angle <- 360/(2+NN) * (1:N+1)
    angle <- angle[idx]
    p$data$angle <- angle
    assign("layout", "fan", envir = p$plot_env)
    return(p)
}

##' @title layout_rectangular
##' @rdname tree-layout
##' @export
layout_rectangular <- function() {
    layout_ggtree('rectangular')
}

##' @title layout_circular
##' @rdname tree-layout
##' @export
layout_circular <- function() {
    layout_ggtree('circular')
}

##' @title layout_inward_circular
##' @rdname tree-layout
##' @export
layout_inward_circular <- function(xlim = NULL) {
    if (!is.null(xlim) && length(xlim) == 1) {
        xlim <- c(xlim, 0)
    }
    layout_ggtree(layout = "inward_circular", xlim = xlim)
}

##' @title layout_fan
##' @rdname tree-layout
##' @param angle open tree at specific angle
##' @export
layout_fan <- function(angle = 180) {
    layout_ggtree('fan', angle = angle)
}
    
##' tree layout
##'
##'
##' @title layout_dendrogram
##' @rdname tree-layout
##' @export
##' @author Guangchuang Yu
layout_dendrogram <- function() {
    layout_ggtree('dendrogram')
}

layout_ggtree <- function(layout = 'rectangular', angle = 180, xlim = NULL) {
    structure(list(layout = layout, angle = angle, xlim = xlim),
              class = 'layout_ggtree')
}

