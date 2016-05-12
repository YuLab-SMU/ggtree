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


layout_circular <- function(treeview) {
    treeview + coord_polar(theta='y', start=-pi/2, -1)
}

##' open tree with specific angle
##'
##' 
##' @title open_tree
##' @param treeview tree view
##' @param angle angle
##' @return updated tree view
##' @export
##' @author Guangchuang Yu
open_tree <- function(treeview, angle) {
    p <- layout_circular(treeview)
    ymax <- max(range(p$data$y))
    p <- p + scale_y_continuous(limits = c(0,
                                           max(c(ymax * (1+angle/(360-angle)), ymax+1))
                                           ))
    N <- nrow(p$data)
    idx <- match(1:N, order(p$data$y))
    NN <- N *(1+angle/(360-angle))
    angle <- 360/(3+NN) * (1:N+1)
    angle <- angle[idx]
    p$data$angle <- angle
    return(p)
}

layout_fan <- open_tree
    
