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

##' add node label for circular layout
##'
##'
##' @title @geom_nodelab2
##' @inheritParams geom_nodelab
##' @return node label layer
##' @export
##' @author Guangchuang Yu
geom_nodelab2 <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, geom = "text", hjust = 0.5, ...) {
    angle <- isTip <- node <- NULL
    m1 <- aes(subset=(!isTip & (angle < 90 | angle > 270)), angle=angle, node = node)
    m2 <- aes(subset=(!isTip & (angle >= 90 & angle <=270)), angle=angle+180, node = node)

    if (!is.null(mapping)) {
        if (!is.null(mapping$subset)) {
            m1 <- aes_string(angle = "angle", node = "node",
                             subset = paste0(as.expression(get_aes_var(mapping, "subset")), '& (!isTip & (angle < 90 | angle > 270))'))
            m2 <- aes_string(angle = "angle+180", node = "node",
                             subset = paste0(as.expression(get_aes_var(mapping, "subset")), '& (!isTip & (angle >= 90 & angle <= 270))'))
        }
        m1 <- modifyList(mapping, m1)
        m2 <- modifyList(mapping, m2)
    }

    list(geom_nodelab(m1, hjust=hjust, nudge_x = nudge_x, nudge_y = nudge_y, geom = geom, ...),
         geom_nodelab(m2, hjust=1-hjust, nudge_x = nudge_x, nudge_y = nudge_y, geom = geom, ...)
         )
}
