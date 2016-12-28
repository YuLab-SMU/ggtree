## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="beast"),
##           function(object, node, group_name="group") {
##               groupClade_(object, node, group_name)
##           })

## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="codeml"),
##           function(object, node, group_name="group") {
##               groupClade_(object, node, group_name)
##           }
##           )

##' groupClade method for ggtree object
##'
##'
##' @name groupClade
##' @title groupClade method
##' @rdname groupClade-methods
##' @param object ggtree object
##' @param node internal node number
##' @param group_name name of the group
##' @importFrom treeio groupClade
##' @exportMethod groupClade
##' @aliases groupClade,ggtree-method
setMethod("groupClade", signature(object="ggtree"),
          function(object, node, group_name) {
              groupClade.ggtree(object, node, group_name)
          })

## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="jplace"),
##           function(object, node, group_name="group") {
##               groupClade_(object, node, group_name)
##           }
##           )

## ##' group selected clade
## ##'
## ##'
## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="nhx"),
##           function(object, node, group_name="group") {
##               groupClade_(object, node, group_name)
##           })

## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="phylip"),
##           function(object, node, group_name="group") {
##               groupClade_(object, node, group_name)
##           })


## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="phylo"),
##           function(object, node, group_name="group") {
##               groupClade.phylo(object, node, group_name)
##           })



## groupClade.phylo <- function(object, node, group_name) {
##     if (length(node) == 1) {
##         clade <- extract.clade(object, node)
##         tips <- clade$tip.label
##     } else {
##         tips <- lapply(node, function(x) {
##             clade <- extract.clade(object, x)
##             clade$tip.label
##         })
##     }

##     groupOTU.phylo(object, tips, group_name)
## }


## groupClade_ <- function(object, node, group_name) {
##     if (is(object, "phylo")) {
##         object <- groupClade.phylo(object, node, group_name)
##     } else {
##         object@phylo <- groupClade.phylo(get.tree(object), node, group_name)
##     }
##     return(object)
## }


groupClade.ggtree <- function(object, nodes, group_name) {
    df <- object$data
    df[, group_name] <- 0
    for (node in nodes) {
        df <- groupClade.df(df, node, group_name)
    }
    df[, group_name] <- factor(df[, group_name])
    object$data <- df
    return(object)
}

groupClade.df <- function(df, node, group_name) {
    foc <- c(node, get.offspring.df(df, node))
    idx <- match(foc, df$node)
    df[idx, group_name] <- max(df[, group_name]) + 1
    return(df)
}
