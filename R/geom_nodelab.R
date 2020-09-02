##' add node label layer
##'
##'
##' @title geom_nodelab
##' @param mapping aes mapping
##' @param nudge_x horizontal adjustment to nudge label
##' @param nudge_y vertical adjustment to nudge label
##' @param geom one of 'text', 'label', 'image' and 'phylopic'
##' @param hjust horizontal alignment, one of 0, 0.5 or 1
##' @param ... additional parameters
##' @return geom layer
##' @export
##' @author Guangchuang Yu
geom_nodelab <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, geom = "text", hjust = 0.5, ...) {
    self_mapping <- aes_(subset = ~!isTip)
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        mapping <- modifyList(self_mapping, mapping)
    }

    geom_tiplab(mapping, offset = nudge_x, nudge_y = nudge_y, geom = geom, hjust = hjust, ...)
}
