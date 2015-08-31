##' add horizontal align lines
##'
##' 
##' @title geom_aline
##' @param mapping aes mapping
##' @param linetype line type
##' @param size line size
##' @param ... additional parameter
##' @return aline layer
##' @export
##' @author Yu Guangchuang
geom_aline <- function(mapping=NULL, linetype="dotted", size=1, ...) {
    x <- y <- isTip <- NULL
    dot_mapping <- aes(xend=x+diff(range(x))/200, x=max(x), yend=y)
    if (!is.null(mapping)) {
        dot_mapping <- modifyList(dot_mapping, mapping)
    }
    
    geom_segment(mapping,
                 subset=.(isTip),
                 linetype=linetype,
                 size=size, ...)
}

