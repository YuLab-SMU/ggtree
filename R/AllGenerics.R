##' @title as.binary
##' @param tree phylo, object
##' @param ... additional parameter
##' @rdname as.binary
##' @export
as.binary <- function(tree, ...) {
    UseMethod("as.binary")
}


##' @docType methods
##' @name reroot
##' @rdname reroot-methods
##' @title reroot method
##' @param object one of \code{phylo}, \code{nhx}, \code{phangorn}, \code{jplace}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
##' @param node internal nnode number
##' @param ... additional parameter
##' @return tree object
##' @importFrom methods setGeneric
##' @export
setGeneric("reroot", function(object, node, ...) standardGeneric("reroot"))


##' @docType methods
##' @name scale_color
##' @rdname scale_color-methods
##' @title scale_color method
##' @param object supported objects, including phylo, paml_rst,
##'               codeml_mlc, codeml, jplace, beast, hyphy
##' @param by one of numerical attributes
##' @param ... additional parameter
##' @return color vector
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

