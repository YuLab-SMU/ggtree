##' scale x for tree with gheatmap
##'
##'
##' @title scale_x_ggtree
##' @param breaks breaks for tree
##' @param labels lables for corresponding breaks
##' @return updated tree view
##' @importFrom ggplot2 waiver
##' @export
##' @author Guangchuang Yu
scale_x_ggtree <- function(breaks = waiver(), labels = waiver()) {
    structure(list(breaks = breaks, labels = labels), class="scale_ggtree")
}


