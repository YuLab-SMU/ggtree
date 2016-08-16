##' add tree scale
##'
##' 
##' @title geom_treescale
##' @param x x position
##' @param y y position
##' @param width width of scale
##' @param offset offset of text to line
##' @param color color
##' @param linesize size of line
##' @param fontsize size of text
##' @param family sans by default, can be any supported font
##' @return ggplot layers
##' @export
##' @author Guangchuang Yu
geom_treescale <- function(x=NULL, y=NULL, width=NULL, offset=NULL, color="black",
                           linesize=0.5, fontsize=3.88, family="sans") {
    
    data=NULL
    position="identity"
    show.legend=NA
    na.rm=TRUE
    inherit.aes=FALSE

    default_aes <- aes_(x=~x, y=~y)
    mapping <- default_aes
    
    list(
        stat_treeScaleLine(xx=x, yy=y, width=width, color=color, offset=offset, size=linesize,
                           mapping=mapping, data=data,
                           position=position, show.legend = show.legend,
                           inherit.aes = inherit.aes, na.rm=na.rm),
        stat_treeScaleText(xx=x, yy=y, width=width, color=color, offset=offset,
                           size=fontsize, family = family, 
                           mapping=mapping, data=data,
                           position=position, show.legend = show.legend,
                           inherit.aes = inherit.aes, na.rm=na.rm)
    )
}



stat_treeScaleLine <- function(mapping=NULL, data=NULL,
                           geom="segment", position="identity",
                           xx, yy, width, offset, color, ..., 
                           show.legend=NA, inherit.aes=FALSE, na.rm=FALSE){
    
    default_aes <- aes_(x=~x, y=~y, xend=~x, yend=~y)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }
    layer(
        stat=StatTreeScaleLine,
        data=data,
        mapping=mapping,
        geom = geom,
        position=position,
        show.legend=show.legend,
        inherit.aes=inherit.aes,
        params=list(xx=xx,
                    yy=yy,
                    width=width,
                    offset=offset,
                    color=color,
                    na.rm=na.rm,
                    ...)
    )
}

stat_treeScaleText <- function(mapping=NULL, data=NULL,
                               geom="text", position="identity",
                               xx, yy, width, offset, color, ...,
                               show.legend=NA, inherit.aes=TRUE, na.rm=FALSE) {

    default_aes <- aes_(x=~x, y=~y, label=~x)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }
    layer(
        stat=StatTreeScaleText,
        data=data,
        mapping=mapping,
        geom=GeomText,
        position=position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(xx=xx,
                      yy=yy,
                      width=width,
                      offset=offset,
                      color=color,
                      na.rm=na.rm,
                      ...)
    )
}


StatTreeScaleLine <- ggproto("StatTreeScaleLine", Stat,
                             compute_group = function(self, data, scales, params, xx, yy, width, offset) {
                                 get_treescale_position(data, xx, yy, width, offset)[[1]]
                             },
                             required_aes = c("x", "y", "xend", "yend")
                             )


StatTreeScaleText <- ggproto("StatTreeScaleText", Stat,
                             compute_group = function(self, data, scales, params, xx, yy, width, offset) {
                                 get_treescale_position(data, xx, yy, width, offset)[[2]]
                             },
                             required_aes = c("x", "y", "label")
                             )



get_treescale_position <- function(data, xx, yy, width, offset=NULL) {
    x <- xx
    y <- yy
    dx <- data$x %>% range %>% diff
    
    if (is.null(x)) {
        x <- dx/2
    }
    
    if (is.null(y)) {
        y <- 0
    }

    if (is.null(width) || is.na(width)) {
        d <- dx/10 
        n <- 0
        while (d < 1) {
            d <- d*10
            n <- n + 1
        }
        d <- floor(d)/(10^n)
    } else {
        d <- width
    }
    
    if (is.null(offset)) {
        offset <- 0.4
    }
    
    list(LinePosition=data.frame(x=x, xend=x+d, y=y, yend=y),
         TextPosition=data.frame(x=x+d/2, y=y+offset, label=d))
}

## ##' add evolution distance legend
## ##'
## ##' 
## ##' @title add_legend
## ##' @param p tree view
## ##' @param width width of legend
## ##' @param x x position
## ##' @param y y position
## ##' @param offset offset of text and line
## ##' @param font.size font size
## ##' @param ... additional parameter
## ##' @return tree view
## ##' @importFrom grid linesGrob
## ##' @importFrom grid textGrob
## ##' @importFrom grid gpar
## ##' @importFrom ggplot2 ylim
## ##' @export
## ##' @author Guangchuang Yu
## add_legend <- function(p, width=NULL, x=NULL, y=NULL, offset=NULL, font.size=4, ...) {
##     dd <- get_treescale_position(p$data, x, y, width, offset)
##     x <- dd[[1]]$x
##     y <- dd[[1]]$y
##     d <- dd[[1]]$xend -x
##     p <- p + annotation_custom(linesGrob(), xmin=x, xmax=x+d, ymin=y, ymax=y) +
##         annotation_custom(textGrob(label=d, gp = gpar(fontsize = font.size)),
##                           xmin=x+d/2, xmax=x+d/2, ymin=y+offset, ymax=y+offset)
##     return(p)
## }


