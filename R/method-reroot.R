
##' @rdname reroot-methods
##' @exportMethod reroot
setMethod("reroot", signature(object="beast"),
          function(object, node, ...) {
              object@phylo <- reroot(object@phylo, node, ...)

              node_map <- attr(object@phylo, "node_map")
              idx <- match(object@stats$node, node_map[,1])
              object@stats$node <- node_map[idx, 2]

              return(object)
          })

## ##' @rdname reroot-methods
## ##' @exportMethod reroot
## setMethod("reroot", signature(object="raxml"),
##           function(object, node, ...) {
##               object@phylo <- reroot(object@phylo, node, ...)

##               node_map <- attr(object@phylo, "node_map")
##               idx <- match(object@bootstrap$node, node_map[,1])
##               object@bootstrap$node <- node_map[idx, 2]

##               return(object)
##           })


##' reroot a tree
##'
##'
##' @rdname reroot-methods
##' @exportMethod reroot
setMethod("reroot", signature(object="phylo"),
          function(object, node, ...) {
              pos <- 0.5* object$edge.length[which(object$edge[,2] == node)]

              ## @importFrom phytools reroot
              phytools <- "phytools"
              require(phytools, character.only = TRUE)

              phytools_reroot <- eval(parse(text="phytools::reroot"))

              tree <- phytools_reroot(object, node, pos)
              attr(tree, "reroot") <- TRUE
              node_map <- reroot_node_mapping(object, tree)
              attr(tree, "node_map") <- node_map
              return(tree)
          })

