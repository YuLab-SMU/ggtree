

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


##' @rdname reroot-methods
##' @exportMethod reroot
setMethod("reroot", signature(object="treedata"),
        function(object, node, ...) {
        	# reroot tree
            tree <- object@phylo
            tree <- reroot(tree, node, ...)
            object@phylo <- tree
            
            # update node numbers in data
            n.tips <- length(tree$tip.label) # Is there a better way in ggtree/treeio to get the number of tips?
            node_map <- attr(tree, "node_map")
            data <- object@data
            data$node[match(node_map$from, as.integer(data$node))] <- node_map$to
            object@data <- data
            
            return(object)
        })
