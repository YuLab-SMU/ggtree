##' @title as.binary
##' @param tree phylo, object
##' @param ... additional parameter
##' @rdname as.binary
##' @export
as.binary <- function(tree, ...) {
    UseMethod("as.binary")
}

##' plot method generics
##'
##'
##' @docType methods
##' @name plot
##' @rdname plot-methods
##' @title plot method
##' @param x object
##' @param ... Additional argument list
##' @return plot
##' @importFrom stats4 plot
##' @export
if ( !isGeneric("plot") )
	setGeneric("plot", function(x, ...) standardGeneric("plot"))


##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @title get.tree method
##' @param object one of \code{jplace}, \code{beast} object
##' @param ... additional parameter
##' @return phylo object
##' @export
setGeneric("get.tree", function(object, ...) standardGeneric("get.tree"))

##' @docType methods
##' @name get.treetext
##' @rdname get.treetext-methods
##' @title get.treetext method
##' @param object one of \code{jplace}, \code{beast} object
##' @param ... additional parameter
##' @return phylo object
##' @export
setGeneric("get.treetext", function(object, ...) standardGeneric("get.treetext"))


##' @docType methods
##' @name get.treeinfo
##' @rdname get.treeinfo-methods
##' @title get.treeinfo method
##' @param object jplace object
##' @param layout layout
##' @param ladderize ladderize, logical
##' @param right logical, parameter for ladderize
##' @param ... additional parameter
##' @return data.frame
##' @export
setGeneric("get.treeinfo", function(object, layout="phylogram", ladderize=TRUE, right=FALSE, ...) standardGeneric("get.treeinfo"))


##' @docType methods
##' @name get.fields
##' @rdname get.fields-methods
##' @title get.fields method
##' @param object one of \code{jplace}, \code{beast} object
##' @param ... additional parameter
##' @return available annotation variables
##' @export
setGeneric("get.fields", function(object, ...) standardGeneric("get.fields"))


##' @docType methods
##' @name get.placements
##' @rdname get.placements-methods
##' @title get.placements method
##' @param object jplace object
##' @param by get best hit or others
##' @param ... additional parameter
##' @return data.frame
##' @export
setGeneric("get.placements", function(object, by, ...) standardGeneric("get.placements"))

##' @docType methods
##' @name get.subs
##' @rdname get.subs-methods
##' @title get.subs method
##' @param object paml_rst object
##' @param type one of 'marginal_subs', 'marginal_AA_subs',
##'                     'joint_subs' or 'joint_AA_subs'.
##' @param ... additional parameter
##' @return data.frame
##' @export
setGeneric("get.subs", function(object, type, ...) standardGeneric("get.subs"))


