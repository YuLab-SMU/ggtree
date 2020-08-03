

##' zoom selected clade of a tree
##'
##'
##' @title geom_zoom_clade
##' @param node internal node number
##' @return updated tree view
##' @author Guangchuang Yu
##' @export
geom_zoom_clade <- function(node) {
    structure(list(node = node), class = "zoom_clade")
}

