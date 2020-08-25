

##' zoom selected clade of a tree
##'
##'
##' @title geom_zoom_clade
##' @param node internal node number
##' @param xexpand numeric, extend x, meaning the ratio of range of original x, 
##' default is NULL.
##' @return updated tree view
##' @author Guangchuang Yu
##' @export
geom_zoom_clade <- function(node, xexpand=NULL) {
    structure(list(node = node, xexpand = xexpand), class = "zoom_clade")
}

