##' @title as.binary
##' @param tree phylo, object
##' @param ... additional parameter
##' @rdname as.binary
##' @export
as.binary <- function(tree, ...) {
    UseMethod("as.binary")
}


##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @title get.tree method
##' @export
setGeneric("get.tree", function(object, ...) standardGeneric("get.tree"))

##' @docType methods
##' @name get.treeinfo
##' @rdname get.treeinfo-methods
##' @title get.treeinfo method
##' @export
setGeneric("get.treeinfo", function(object, layout="phylogram", ladderize=TRUE, right=FALSE, ...) standardGeneric("get.treeinfo"))


##' @docType methods
##' @name get.fields
##' @rdname get.fields-methods
##' @title get.fields method
##' @param object one of \code{jplace}, \code{beast} object
##' @param ... additional parameter
##' @export
setGeneric("get.fields", function(object, ...) standardGeneric("get.fields"))


##' @docType methods
##' @name get.placements
##' @rdname get.placements-methods
##' @title get.placements method
##' @export
setGeneric("get.placements", function(object, by, ...) standardGeneric("get.placements"))

##' @docType methods
##' @name "set.treeinfo<-"
##' @rdname set.treeinfo-methods
##' @title "set.treeinfo<-" method
##' @export
setGeneric("set.treeinfo<-", function(x, value) standardGeneric("set.treeinfo<-"))
