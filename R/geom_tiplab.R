##' add tip label layer
##'
##' 
##' @title geom_tiplab 
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param align align tip lab or not, logical
##' @param linetype linetype for adding line if align = TRUE
##' @param linesize line size of line if align = TRUE
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab()
geom_tiplab <- function(mapping=NULL, hjust = 0, align = FALSE, linetype = "dotted", linesize=1, ...) {
    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        self_mapping <- aes(x = max(x) + diff(range(x, na.rm=TRUE))/200, y = y, label = label, subset= isTip)
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
        dot_mapping <- aes(xend=x+diff(range(x))/200, x=max(x), y=y, yend=y, subset=isTip)
        if (!is.null(mapping)) {
            dot_mapping <- modifyList(dot_mapping, mapping)
        }
    } 
    
    list(
        geom_text(mapping=text_mapping,
                  subset=.(isTip),
                  hjust = hjust, ...),
        if (!is.null(dot_mapping))
            geom_segment(mapping=dot_mapping,
                         subset=.(isTip),
                          linetype = linetype,
                          size = linesize, ...)
        )
}




