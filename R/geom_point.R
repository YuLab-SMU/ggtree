##' add tip point
##'
##' 
##' @title geom_tippoint
##' @param mapping aes mapping
##' @param ... additional parameter
##' @return tip point layer
##' @export
##' @author Guangchuang Yu
geom_tippoint <- function(mapping = NULL, ...) {
    isTip <- NULL
    geom_point(mapping, subset=.(isTip), ...)
}

##' add node point
##'
##' 
##' @title geom_nodepoint
##' @param mapping aes mapping
##' @param ... additional parameter
##' @return node point layer
##' @export
##' @author Guangchuang Yu
geom_nodepoint <- function(mapping = NULL,  ...) {
    isTip <- NULL
    geom_point(mapping, subset=.(!isTip), ...)
}



##' add root point
##'
##' 
##' @title geom_rootpoint
##' @param mapping aes mapping
##' @param ... additional parameter
##' @return root point layer
##' @importFrom ggplot2 geom_point
##' @export
##' @author Guangchuang Yu
geom_rootpoint <- function(mapping = NULL,  ...) {
    isTip <- node <- parent <- NULL
    geom_point(mapping, subset=.(node == parent), ...)
}


