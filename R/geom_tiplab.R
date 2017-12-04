##' add tip label layer
##'
##'
##' @title geom_tiplab
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param offset tiplab offset
##' @param align align tip lab or not, logical
##' @param linetype linetype for adding line if align = TRUE
##' @param linesize line size of line if align = TRUE
##' @param geom one of 'text', 'label', 'image' and 'phylopic'
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @importFrom utils modifyList
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab()
geom_tiplab <- function(mapping=NULL, hjust = 0,  align = FALSE, linetype = "dotted", linesize=0.5, geom="text",  offset=0, ...) {
    geom <- match.arg(geom, c("text", "label", "image", "phylopic"))
    if (geom == "text") {
        label_geom <- geom_text2
    } else if (geom == "label") {
        label_geom <- geom_label2
    } else if (geom == "image") {
        label_geom <- get_fun_from_pkg("ggimage", "geom_image")
    } else if (geom == "phylopic") {
        label_geom <- get_fun_from_pkg("ggimage", "geom_phylopic")
    }


    x <- y <- label <- isTip <- node <- NULL
    if (align == TRUE) {
        self_mapping <- aes(x = max(x, na.rm=TRUE) + diff(range(x, na.rm=TRUE))/200, y = y, label = label, node = node, subset = isTip)
    }
    else {
        self_mapping <- aes(x = x + diff(range(x, na.rm=TRUE))/200, y= y, label = label, node = node, subset = isTip)
    }

    if (is.null(mapping)) {
        text_mapping <- self_mapping
    } else {
        text_mapping <- modifyList(self_mapping, mapping)
    }


    show_segment <- FALSE
    if (align && (!is.na(linetype) && !is.null(linetype))) {
        show_segment <- TRUE
        segment_mapping <- aes(x = max(x, na.rm=TRUE),
                               xend = x + diff(range(x, na.rm=TRUE))/200,
                               y = y, yend = y,
                               node = node,
                               label = label,
                               subset = isTip)
        if (!is.null(mapping))
            segment_mapping <- modifyList(segment_mapping, mapping)
    }

    list(
        if (show_segment)
            geom_segment2(mapping = segment_mapping,
                          linetype = linetype, nudge_x = offset,
                          size = linesize, stat = StatTreeData, ...)
       ,
        label_geom(mapping=text_mapping,
                   hjust = hjust, nudge_x = offset, stat = StatTreeData, ...)

    )
}


##' add tip label for circular layout
##'
##'
##' @title geom_tiplab2
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param ... additional parameter, see geom_tiplab
##' @return tip label layer
##' @export
##' @author Guangchuang Yu
##' @references \url{https://groups.google.com/forum/#!topic/bioc-ggtree/o35PV3iHO-0}
geom_tiplab2 <- function(mapping=NULL, hjust=0, ...) {

    angle <- isTip <- node <- NULL

    ## m1 <- aes(subset=(abs(angle) < 90), angle=angle)
    ## m2 <- aes(subset=(abs(angle) >= 90), angle=angle+180)
    m1 <- aes(subset=(isTip & (angle < 90 | angle > 270)), angle=angle, node = node)
    m2 <- aes(subset=(isTip & (angle >= 90 & angle <=270)), angle=angle+180, node = node)

    if (!is.null(mapping)) {
        m1 <- modifyList(mapping, m1)
        m2 <- modifyList(mapping, m2)
    }

    list(geom_tiplab(m1, hjust=hjust, ...),
         geom_tiplab(m2, hjust=1-hjust, ...)
         )
}



