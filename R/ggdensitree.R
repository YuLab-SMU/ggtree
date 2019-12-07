##' drawing phylogenetic trees from list of phylo objects
##'
##' @title ggdensitree
##' @param data a list of phylo objects or any object with an as.phylo and fortify method
##' @param mapping aesthetic mapping
##' @param layout one of 'slanted', 'rectangluar', 'fan', 'circular' or 'radial' (default: 'slanted')
##' @param tip.order the order of the tips by a character vector of taxa names; or an integer, N, to order the tips by the order of the tips in the Nth tree; 'mds' to order the tips based on MDS of the path length between the tips;  or 'mds_dist' to order the tips based on MDS of the distance between the tips (default: 'mds_dist')
##' @param align.tips TRUE to align trees by their tips and FALSE to align trees by their root (default: TRUE)
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
##' 
##' # Plot mutliple trees with aligned tips
##' trees <- list(read.tree(text="((a:1,b:1):1.5,c:2.5);"), read.tree(text="((a:1,c:1):1,b:2);"));
##' ggdensitree(trees) + geom_tiplab()
##' 
##' # Plot multiple trees with aligmned tips with tip labls and separate tree colors
##' trees.fort <- list(trees[[1]] %>% fortify %>% mutate(tree="a"), trees[[2]] %>% fortify %>% mutate(tree="b"));
##' ggdensitree(trees.fort, aes(colour=tree)) + geom_tiplab(colour='black')
##' 
##' 
##' # Generate example data
##' set.seed(1)
##' trees <- rmtree(5, 10)
##' time.trees <- lapply(1:length(trees), function(i) {
##'  	tree <- trees[[i]]
##'  	tree$tip.label <- paste0("t", 1:10)
##' 	dates <- estimate.dates(tree, 1:10, mu=1, nsteps=1)
##' 	tree$edge.length <- dates[tree$edge[, 2]] - dates[tree$edge[, 1]]
##' 	fortify(tree) %>% mutate(tree=factor(i, levels=as.character(1:10)))
##' })
##' 
##' # Plot multiple trees with aligned tips from muliple time points
##' ggdensitree(time.trees, aes(colour=tree), tip.order=paste0("t", 1:10)) + geom_tiplab(colour='black')
##' 
##' 
##' # Read example data
##' trees <- read.tree(system.file("examples", "ggdensitree_example.tree", package="ggtree"))
##' 
##' # Compute OTU
##' grp <- list(A = c("a.t1", "a.t2", "a.t3", "a.t4"), B = c("b.t1", "b.t2", "b.t3", "b.t4"), C = c("c.t1", "c.t2", "c.t3", "c.t4"))
##' trees <- lapply(trees, groupOTU, grp)
##' 
##' # Plot multiple trees colored by OTU
##' ggdensitree(trees, aes(colour=group), alpha=1/6) + scale_colour_manual(values=c("black", "red", "green", "blue"))
ggdensitree <- function(data=NULL, mapping=NULL, layout="slanted", tip.order='mds_dist',
						align.tips=TRUE, jitter=0, ...) {
	## factorize to simplify code
	trees <- lapply(data, as.phylo)
	trees.f <- lapply(data, fortify, layout=layout, ...)
	n.tips <- sum(trees.f[[1]]$isTip)
	
	## determine tip order
	if (length(tip.order) == 1) {
		if (grepl('mds', tip.order)) {
			method <- tip.order
			
			first.label <- trees.f[[1]] %>%
				dplyr::filter(.data$isTip) %>%
				dplyr::pull(.data$label)
			
			tip.order <- lapply(trees, function(x) {
				match(x$tip.label, first.label)
			})
			
			tip.2.tip <- lapply(trees, cophenetic.phylo.check.length, method=method)
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
	
	## reorder tips (and shift x id align tips)
	max.x <- vapply(trees.f, function(x) max(x$x, na.rm = TRUE), numeric(1))
	farthest <- max(max.x)
	trees.f <- lapply(1:length(trees), function(i) {
		trees.f[[i]]$y <- getYcoord(trees[[i]], tip.order = tip.order)
		if (align.tips) {
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
cophenetic.phylo.check.length <- function(tree, method) {
	if (method != 'mds_dist' || is.null(tree$edge.length))
		tree$edge.length <- rep(1, nrow(tree$edge))
	
	stats::cophenetic(tree)
}