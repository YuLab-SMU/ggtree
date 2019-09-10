##' drawing phylogenetic trees from list of phylo objects
##'
##' @title ggdensitree
##' @param data a list of phylo objects or any object with an as.phylo and fortify method
##' @param mapping aesthetic mapping
##' @param layout one of 'slanted', 'rectangluar', 'fan', 'circular' or 'radial' (default: 'slanted')
##' @param tip.order the order of the tips by a character vector of taxa names; or an integer, N, to order the tips by the order of the tips in the Nth tree; or 'mds' to order the tips based on MDS of the distance between the tips (default: 'mds')
##' @param dangle TRUE to align trees by their tips and FALSE to align treesby their root (default: TRUE)
##' @param jitter deviation to jitter tips
##' @param ... additional parameters passed to fortify, ggtree and geom_tree
##' @return tree layer
##' @importFrom magrittr %<>%
##' @importFrom magrittr add
##' @export
##' @author Yu Guangchuang, Bradley R. Jones
##' @examples
##' require(ape)
##' require(dplyr)
##' library(ape)
##' library(tidyverse)
##' 
##' trees <- list(read.tree(text="((a,b),c);"), read.tree(text="((a,c),b);"));
##' ggdensitree(trees) + geom_tiplab()
##' 
##' trees.fort <- list(trees[[1]] %>% fortify %>% mutate(tree="a"), trees[[2]] %>% fortify %>% mutate(tree="b"));
##' ggdensitree(trees.fort, aes(colour=tree)) + geom_tiplab(colour='black')
ggdensitree <- function(data=NULL, mapping=NULL, layout="slanted", tip.order='mds', dangle=TRUE, jitter=0, ...) {
	# factorize to simplify code
	trees <- lapply(data, as.phylo)
	trees.f <- lapply(data, fortify, layout=layout, ...)
	n.tips <- sum(trees.f[[1]]$isTip)
	
	# determine tip order
	if (length(tip.order) == 1) {
		if (tip.order == 'mds') {
			first.label <- subset(trees.f[[1]], isTip)$label
			
			tip.order <- lapply(trees, . %$% tip.label %>% match(first.label))
			
			tip.2.tip <- lapply(trees, cophenetic.phylo.check.length)
			tip.2.tip <- lapply(1:length(trees), function(i) tip.2.tip[[i]][tip.order[[i]], tip.order[[i]]])
			
			all.tab <- do.call(rbind, tip.2.tip)
			rownames(all.tab) <- NULL
			
			distances <- dist(t(all.tab))
			
			res <- cmdscale(distances, k=1)
			
			tip.order <- first.label[order(res[,1])]
		} else if (as.numeric(tip.order) && tip.order <= length(trees)) {
			labels <- subset(trees.f[[tip.order]], isTip)$label
			
			tip.order <- labels[as.integer(trees.f[[tip.order]]$y)]
		}
	}
	
	# reorder tips (and shift x id dangling)
    trees.f <- lapply(1:length(trees), function(i) {
    	trees.f[[i]]$y <- getYcoord_order(trees[[i]], tip.order)
    	if (i > 1) {
    		trees.f[[i]]$y[1:n.tips] %<>% add(rnorm(n.tips, mean=0, sd=jitter))
    		if (dangle)
   	    		trees.f[[i]]$x <- trees.f[[1]]$x[1] - trees.f[[i]]$x[match(trees.f[[1]]$label, trees.f[[i]]$label)][1] + trees.f[[i]]$x
    	}
    	trees.f[[i]]
    })
	
    # plot all trees together
	p <- ggtree(tr=trees.f[[1]], mapping=mapping, layout=layout, ...)
	for (x in trees.f[-1])
		p <- p + geom_tree(mapping=mapping, data=x, layout=layout, ...)
	p
}

## wrapper for cohpenetic to ensure that branch lengths exist
cophenetic.phylo.check.length <- function(tree) {
	if (is.null(tree$edge.length))
		tree$edge.length <- rep(1, nrow(tree$edge))
	
	cophenetic(tree)
}

## this is an adaptation of old code in ggtree
## 
##' @importFrom magrittr %>%
##' @importFrom magrittr equals
getYcoord_order <- function(tr, tip.order) {
    Ntip <- length(tr[["tip.label"]])
    N <- getNodeNum(tr)

    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]

    cl <- split(child, parent)
    child_list <- list()
    child_list[as.numeric(names(cl))] <- cl

    y <- numeric(N)
    y[1:Ntip] <- match(tr$tip.label, tip.order)
    y[-(1:Ntip)] <- NA

    pvec <- integer(max(tr$edge))
    pvec[child] = parent

    currentNode <- 1:Ntip
    while(anyNA(y)) {
        pNode <- unique(pvec[currentNode])

        idx <- sapply(pNode, function(i) all(child_list[[i]] %in% currentNode))
        newNode <- pNode[idx]

        y[newNode] <- sapply(newNode, function(i) {
            mean(y[child_list[[i]]], na.rm=TRUE)
        })

        currentNode <- c(currentNode[!currentNode %in% unlist(child_list[newNode])], newNode)
    }

    return(y)
}
