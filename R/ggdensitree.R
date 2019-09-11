##' drawing phylogenetic trees from list of phylo objects
##'
##' @title ggdensitree
##' @param data a list of phylo objects or any object with an as.phylo and fortify method
##' @param mapping aesthetic mapping
##' @param layout one of 'slanted', 'rectangluar', 'fan', 'circular' or 'radial' (default: 'slanted')
##' @param branch.length variable to be used to scale branch length. Setting to 'branch.length' will use the branch lengths of the tree objects. Default is 'none' to discard branch length and only plot cladogram (more reasonable for densitree).
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
##' 
##' trees <- list(read.tree(text="((a,b),c);"), read.tree(text="((a,c),b);"));
##' ggdensitree(trees) + geom_tiplab()
##' 
##' trees.fort <- list(trees[[1]] %>% fortify %>% mutate(tree="a"), trees[[2]] %>% fortify %>% mutate(tree="b"));
##' ggdensitree(trees.fort, aes(colour=tree)) + geom_tiplab(colour='black')
ggdensitree <- function(data=NULL, mapping=NULL, layout="slanted", branch.length = "none",
                        tip.order='mds', dangle=TRUE, jitter=0, ...) {
    ## factorize to simplify code
    trees <- lapply(data, as.phylo)
    trees.f <- lapply(data, fortify, layout=layout, branch.length = branch.length, ...)
    n.tips <- sum(trees.f[[1]]$isTip)

    ## determine tip order
    if (length(tip.order) == 1) {
        if (tip.order == 'mds') {
            first.label <- trees.f[[1]] %>%
                dplyr::filter(.data$isTip) %>%
                dplyr::pull(.data$label)
            
            tip.order <- lapply(trees, function(x) {
                match(x$tip.label, first.label)
            })
            
            tip.2.tip <- lapply(trees, cophenetic.phylo.check.length)
            tip.2.tip <- lapply(1:length(trees), function(i) {
                tip.2.tip[[i]][tip.order[[i]], tip.order[[i]]]
            })
            
            all.tab <- do.call(rbind, tip.2.tip)
            rownames(all.tab) <- NULL
            
            distances <- stats::dist(t(all.tab))
            
            res <- stats::cmdscale(distances, k=1)
			
            tip.order <- first.label[order(res[,1])]
        } else if (as.numeric(tip.order) && tip.order <= length(trees)) {
            labels <- trees.f[[tip.order]] %>%
                dplyr::filter(.data$isTip) %>%
                dplyr::pull(.data$label)
            
            tip.order <- labels[as.integer(trees.f[[tip.order]]$y)]
        }
    }
	
    ## reorder tips (and shift x id dangling)
    max.x <- vapply(trees.f, function(x) max(x$x, na.rm = TRUE), numeric(1))
    farthest <- max(max.x)
    trees.f <- lapply(1:length(trees), function(i) {
        trees.f[[i]]$y <- getYcoord(trees[[i]], tip.order = tip.order)
        if (dangle) {
            trees.f[[i]]$x <- trees.f[[i]]$x + (farthest - max.x[i])
        }
        if (i > 1 && jitter > 0) {
            trees.f[[i]]$y[1:n.tips] %<>% add(stats::rnorm(n.tips, mean=0, sd=jitter))
        }
        trees.f[[i]]
    })
    
    ## plot all trees together
    p <- ggtree(tr=trees.f[[1]], mapping=mapping, layout=layout, ...)
    for (x in trees.f[-1])
        p <- p + geom_tree(mapping=mapping, data=x, layout=layout, ...)
    p
}

## wrapper for cohpenetic to ensure that branch lengths exist
cophenetic.phylo.check.length <- function(tree) {
    if (is.null(tree$edge.length))
        tree$edge.length <- rep(1, nrow(tree$edge))
    
    stats::cophenetic(tree)
}
