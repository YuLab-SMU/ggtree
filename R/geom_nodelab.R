##' add node label layer for a tree
##'
##'
##' @title geom_nodelab
##' @param mapping aesthetic mappings, defaults to NULL
##' @param nudge_x horizontal adjustment to nudge labels, defaults to 0
##' @param nudge_y vertical adjustment to nudge labels, defaults to 0
##' @param geom one of 'text', "shadowtext", 'label', 'image' and 'phylopic'
##' @param hjust horizontal alignment, defaults to 0.5
##' @param node a character indicating which node labels will be displayed,
##' it should be one of 'internal', 'external' and 'all'. If it is set to 'internal'
##' will display internal node labels, 'external' will display the tip labels,
##' and 'all' will display internal node and tip labels.
##' @param ... additional parameters, see also 
##' the additional parameters of [geom_tiplab()].
##' @seealso [geom_tiplab()]
##' @return geom layer
##' @export
##' @author Guangchuang Yu
##' @references    
##'  For demonstration of this function, please refer to chapter A.4.5 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_nodelab <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, geom = "text", hjust = 0.5, node="internal",...) {

    p <- geom_tiplab(mapping, offset = nudge_x, nudge_y = nudge_y, geom = geom, hjust = hjust, ...)
    p$node <- match.arg(node, c("internal", "external", "all"))
    return (p)
}
