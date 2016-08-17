##' add tip label layer
##'
##' 
##' @title geom_tiplab 
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param align align tip lab or not, logical
##' @param linetype linetype for adding line if align = TRUE
##' @param linesize line size of line if align = TRUE
##' @param geom one of 'text' and 'label'
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab()
geom_tiplab <- function(mapping=NULL, hjust = 0, align = FALSE, linetype = "dotted", linesize=1, geom="text", ...) {
    geom <- match.arg(geom, c("text", "label"))

    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        self_mapping <- aes(x = max(x, na.rm=TRUE) + diff(range(x, na.rm=TRUE))/200, y = y, label = label, subset= isTip)
    }
    else {
        self_mapping <- aes(x = x + diff(range(x, na.rm=TRUE))/200, y= y, label = label, subset= isTip)
    }

    if (is.null(mapping)) {
        text_mapping <- self_mapping          
    } else {
        text_mapping <- modifyList(self_mapping, mapping)
    }

    dot_mapping <- NULL
    if (align && (!is.na(linetype) && !is.null(linetype))) {
        dot_mapping <- aes(xend=x+diff(range(x, na.rm=TRUE))/200, x=max(x, na.rm=TRUE), y=y, yend=y, subset=isTip)
        if (!is.null(mapping)) {
            dot_mapping <- modifyList(dot_mapping, mapping)
        }
    } 
    
    list(
        if (geom == "text") {
            geom_text2(mapping=text_mapping, 
                       hjust = hjust, ...)
        } else {
            geom_label2(mapping=text_mapping, 
                        hjust = hjust, ...)
        },
        if (!is.null(dot_mapping))
            geom_segment2(mapping=dot_mapping,
                          linetype = linetype,
                          size = linesize, ...)
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

    angle <- NULL
    ## m1 <- aes(subset=(abs(angle) < 90), angle=angle)
    ## m2 <- aes(subset=(abs(angle) >= 90), angle=angle+180)
    m1 <- aes(subset=(angle < 90 | angle > 270), angle=angle)
    m2 <- aes(subset=(angle >= 90 & angle <=270), angle=angle+180)
    
    if (!is.null(mapping)) {
        m1 <- modifyList(mapping, m1)
        m2 <- modifyList(mapping, m2)
    }

    list(geom_tiplab(m1, hjust=hjust, ...),
         geom_tiplab(m2, hjust=1-hjust, ...)
         )
}
