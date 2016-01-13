##' view a clade of tree
##'
##' 
##' @title viewClade
##' @param tree_view full tree view 
##' @param node internal node number
##' @return clade plot
##' @export
##' @author Guangchuang Yu
viewClade <- function(tree_view, node) {
    cpos <- get_clade_position(tree_view, node=node)
    with(cpos, p+xlim(xmin, xmax*1.01)+ylim(ymin, ymax))
}
