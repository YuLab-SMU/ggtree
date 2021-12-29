##' rotate circular tree
##'
##' 
##' @title rotate_tree
##' @param treeview tree view
##' @param angle the angle of rotation
##' @return updated tree view
##' @export
##' @examples
##' tree <- rtree(15)
##' p <- ggtree(tree) + geom_tiplab()
##' p2 <- open_tree(p, 180)
##' p3 <- rotate_tree(p2, 180)
##' p3
##' @author Guangchuang Yu
rotate_tree <- function(treeview, angle) {
    treeview <- treeview + coord_polar(theta='y', start=(angle-90)/180*pi, -1)
    treeview$data$angle <- treeview$data$angle + angle
    assign("layout", "circular", envir = treeview$plot_env)
    return(treeview)
}

##' transform a tree in either rectangular or circular layout into the fan layout that opens with a specific angle
##'
##' 
##' @title open_tree
##' @param treeview tree view
##' @param angle open the tree at a specific angle
##' @return updated tree view
##' @importFrom ggplot2 scale_y_continuous
##' @export
##' @examples
##' tree <- rtree(15)
##' p <- ggtree(tree) + geom_tiplab()
##' open_tree(p, 180)
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

##' transform circular/fan layout to rectangular layout
##'
##' 
##' @title layout_rectangular
##' @rdname tree-layout
##' @export
##' @examples
##' tree <- rtree(20)
##' ggtree(tree, layout = "circular") + layout_rectangular()
layout_rectangular <- function() {
    layout_ggtree('rectangular')
}

##' transform rectangular layout to circular layout
##'
##' 
##' @title layout_circular
##' @rdname tree-layout
##' @export
##' @examples
##' tree <- rtree(20)
##' p <- ggtree(tree)
##' p + layout_circular()
layout_circular <- function() {
    layout_ggtree('circular')
}

##' transform rectangular/circular layout to inward_circular layout
##'
##' 
##' @title layout_inward_circular
##' @param xlim setting x limits, which will affect the center space of the tree
##' @rdname tree-layout
##' @export
##' @examples
##' tree <- rtree(20)
##' p <- ggtree(tree)
##' p + layout_inward_circular(xlim=4) + geom_tiplab(hjust=1)
layout_inward_circular <- function(xlim = NULL) {
    if (!is.null(xlim) && length(xlim) == 1) {
        xlim <- c(xlim, 0)
    }
    layout_ggtree(layout = "inward_circular", xlim = xlim)
}

##' transform rectangular/circular layout to fan layout
##'
##' 
##' @title layout_fan
##' @rdname tree-layout
##' @param angle open tree at specific angle
##' @export
##' @examples
##' tree <- rtree(20)
##' p <- ggtree(tree)
##' p + layout_fan(angle=90)
layout_fan <- function(angle = 180) {
    layout_ggtree('fan', angle = angle)
}

##' transform rectangular layout to dendrogram layout
##'
##'
##' @title layout_dendrogram
##' @rdname tree-layout
##' @export
##' @examples
##' tree <- rtree(20)
##' p <- ggtree(tree)
##' p + p + layout_dendrogram()
##' @author Guangchuang Yu
layout_dendrogram <- function() {
    layout_ggtree('dendrogram')
}

layout_ggtree <- function(layout = 'rectangular', angle = 180, xlim = NULL) {
    structure(list(layout = layout, angle = angle, xlim = xlim),
              class = 'layout_ggtree')
}

