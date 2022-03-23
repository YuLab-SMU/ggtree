##' scale x for tree with gheatmap
##' 
##' Since setting x-axis for tree with gheatmap by using 'theme_tree2()' is quite tricky,
##' 'scale_x_ggtree' can help set the x-axis more reasonably.
##'
##'
##' @title scale_x_ggtree
##' @param breaks set breaks for tree
##' @param labels lables for corresponding breaks
##' @return updated tree view
##' @importFrom ggplot2 waiver
##' @export
##' @author Guangchuang Yu
##' @references
##' For more detailed demonstration of this function, please refer to chapter 7.3 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
scale_x_ggtree <- function(breaks = waiver(), labels = waiver()) {
    structure(list(breaks = breaks, labels = labels), class="scale_ggtree")
}


