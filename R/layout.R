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
    return(p)
}

##' @title layout_rectangular
##' @rdname tree-layout
##' @export
layout_rectangular <- function() {
    coord_cartesian()
}

##' @title layout_circular
##' @rdname tree-layout
##' @export
layout_circular <- function() {
    coord_polar(theta='y', start=-pi/2, -1)
}

##' @title layout_fan
##' @rdname tree-layout
##' @param angle open tree at specific angle
##' @export
layout_fan <- function(angle = 180) {
    structure(list(angle = angle), class = "layout_fan")
}
    
##' tree layout
##'
##'
##' @title layout_dendrogram
##' @rdname tree-layout
##' @export
##' @author Guangchuang Yu
layout_dendrogram <- function() {
    structure(list(), class = 'layout_dendrogram')
}


