

##' zoom selected clade of a tree
##'
##' 'geom_zoom_clade' zooms in on a selected clade of a tree, 
##' while showing its on the full view of tree as a seperated panel for reference
##'
##' @title geom_zoom_clade
##' @param node internal node number to zoom in its corresponding clade
##' @param xexpand numeric, extend x, meaning the ratio of range of the xlim of the original tree, 
##' defaults to NULL.
##' @return updated tree view
##' @author Guangchuang Yu
##' @export
geom_zoom_clade <- function(node, xexpand=NULL) {
    structure(list(node = node, xexpand = xexpand), class = "zoom_clade")
}

