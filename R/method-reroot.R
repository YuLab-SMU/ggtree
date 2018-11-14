

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
        	
        	newobject <- object
        	
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
            newobject@phylo <- tree
            
            # update node numbers in data
            n.tips <- Ntip(tree)
            node_map<- attr(tree, "node_map")
            
            update_data <- function(data, node_map) {
            	newdata <- data
            	newdata[match(node_map$from, data$node), 'node'] <- node_map$to
            	
            	# clear root data
            	root <- newdata$node == (n.tips + 1)
            	newdata[root,] <- NA
            	newdata[root,'node'] <- n.tips + 1
            	
            	return(newdata)
            }
            
            if (nrow(newobject@data) > 0) {
            	newobject@data <- update_data(object@data, node_map)
            }
            
            if (nrow(object@extraInfo) > 0) {
            	newobject@extraInfo <- update_data(object@extraInfo, node_map)
            }
            
            return(newobject)
        })
