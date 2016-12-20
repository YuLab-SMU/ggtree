## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="apeBootstrap"),
##           function(object,...) {
##               object@phylo
##           }
##           )

## ##' get.tree method
## ##'
## ##'
## ##' @docType methods
## ##' @name get.tree
## ##' @rdname get.tree-methods
## ##' @aliases get.tree,beast
## ##' @exportMethod get.tree
## ##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
## ##' @usage get.tree(object, ...)
## setMethod("get.tree", signature(object="beast"),
##           function(object,...) {
##               object@phylo
##           }
##           )


## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## ##' @param by one of rst or mlc
## setMethod("get.tree", signature(object="codeml"),
##           function(object, by="rst", ...) {
##               if (by == "rst") {
##                   return(object@rst@phylo)
##               } else {
##                   return(object@mlc@phylo)
##               }
##           })


## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="jplace"),
##           function(object) {
##               object@phylo
##           })

## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object = "nhx"),
##           function(object) {
##               object@phylo
##           }
##           )

## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="phylip"),
##           function(object,...) {
##               object@phylo
##           }
##           )

## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="phylo"),
##           function(object, ...) {
##               return(object)
##           })

## ##' @rdname get.tree-methods
## ##' @exportMethod get.tree
## setMethod("get.tree", signature(object="data.frame"),
##           function(object, ...) {
##               edge <- object[, c("parent", "node")]
##               i <- which(edge[,1] != 0 & edge[,1] != edge[,2])
##               edge <- edge[i, ]
##               edge.length <- object[i, "branch.length"]
##               tip.label <- object[object[, "isTip"], "label"]
##               phylo <- list(edge = as.matrix(edge),
##                   edge.length = edge.length,
##                   tip.label = tip.label)

##               node.label <- object[!object[, "isTip"], "label"]
##               if (!all(is.na(node.label))) {
##                   phylo$node.label <- node.label
##               }
##               phylo$Nnode <- sum(!object[, "isTip"])
##               class(phylo) <- "phylo"
##               return(phylo)
##           })

