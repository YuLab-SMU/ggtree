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
##' @name reroot
##' @rdname reroot-methods
##' @title reroot method
##' @param object one of \code{phylo}, \code{nhx}, \code{phangorn}, \code{jplace}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
##' @param node internal nnode number
##' @param ... additional parameter
##' @return tree object
##' @export
setGeneric("reroot", function(object, node, ...) standardGeneric("reroot"))

##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @title get.tree method
##' @param object one of \code{phylo}, \code{jplace}, \code{nhx}, \code{phangorn}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
##' @param ... additional parameter
##' @return phylo object
##' @export
setGeneric("get.tree", function(object, ...) standardGeneric("get.tree"))

##' @docType methods
##' @name get.treetext
##' @rdname get.treetext-methods
##' @title get.treetext method
##' @param object one of \code{phylo}, \code{jplace}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
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
##' @param object one of \code{jplace}, \code{beast}, \code{hyphy}, \code{codeml}, \code{codeml_mlc}, \code{paml_rst} object
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


##' @docType methods
##' @name get.tipseq
##' @rdname get.tipseq-methods
##' @title get.tipseq method
##' @param object one of paml_rst or codeml object
##' @param ... additional parameter
##' @return character
##' @export
setGeneric("get.tipseq", function(object, ...) standardGeneric("get.tipseq"))

##' @docType methods
##' @name groupOTU
##' @rdname groupOTU-methods
##' @title groupOTU method
##' @param object supported objects, including phylo, paml_rst,
##'               codeml_mlc, codeml, jplace, beast, hyphy
##' @param focus a vector of tip (label or number) or a list of tips.
##' @param group_name name of the group, 'group' by default
##' @param ... additional parameter
##' @return group index
##' @export
setGeneric("groupOTU", function(object, focus, group_name="group", ...) standardGeneric("groupOTU"))

##' @docType methods
##' @name groupClade
##' @rdname groupClade-methods
##' @title groupClade method
##' @param object supported objects, including phylo, paml_rst,
##'               codeml_mlc, codeml, jplace, beast, hyphy
##' @param node a internal node or a vector of internal nodes
##' @param group_name name of the group, 'group' by default
##' @param ... additional parameter
##' @return group index
##' @export
setGeneric("groupClade", function(object, node, group_name="group", ...) standardGeneric("groupClade"))


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
