

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
        	# warning message
        	message("The use of this method may cause some node data to become incorrect (e.g. bootstrap values).")
        	
        	# ensure nodes/tips have a label to properly map @anc_seq/@tip_seq
        	tree <- object@phylo
        	if (is.null(tree$tip.label)) {
        		tree$tip.label <- as.character(1:Ntip(tree))
        	}
        	if (is.null(tree$node.label)) {
        		tree$node.label <- as.character((1:tree$Nnode) + Ntip(tree))
        	}
        	
            # reroot tree
            tree <- reroot(tree, node, ...)
            object@phylo <- tree
            
            # update node numbers in data
            n.tips <- Ntip(tree)
            node_map <- attr(tree, "node_map")
            data <- object@data
            data$node[match(node_map$from, as.integer(data$node))] <- node_map$to
            object@data <- data
            
            return(object)
        })
