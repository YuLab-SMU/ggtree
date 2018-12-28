##' @importFrom methods setOldClass
setOldClass("ggtree")

##' @docType methods
##' @name scale_color
##' @rdname scale_color-methods
##' @title scale_color method
##' @param object \code{treedata} object
##' @param by one of numerical attributes
##' @param ... additional parameter
##' @return color vector
##' @importFrom methods setGeneric
##' @export
setGeneric("scale_color", function(object, by, ...) standardGeneric("scale_color"))

##' @docType methods
##' @name gzoom
##' @rdname gzoom-methods
##' @title gzoom method
##' @param object supported tree objects
##' @param focus selected tips
##' @param subtree logical
##' @param widths widths
##' @param ... additional parameter
##' @return figure
##' @export
setGeneric("gzoom", function(object, focus, subtree=FALSE, widths=c(.3, .7), ...) standardGeneric("gzoom"))

