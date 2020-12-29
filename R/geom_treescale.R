##' add tree scale
##'
##'
##' @title geom_treescale
##' @param x x position
##' @param y y position
##' @param width width of scale
##' @param offset offset of text to line
##' @param label the title of tree scale, default is NULL.
##' @param offset.label offset of scale title to line.
##' @param color color
##' @param linesize size of line
##' @param fontsize size of text
##' @param family sans by default, can be any supported font
##' @return ggplot layers
##' @export
##' @author Guangchuang Yu
geom_treescale <- function(x=NULL, y=NULL, width=NULL, offset=NULL, 
                           offset.label=NULL, label=NULL, color="black",
                           linesize=0.5, fontsize=3.88, family="sans") {

    data=NULL
    position="identity"
    show.legend=NA
    na.rm=TRUE
    inherit.aes=FALSE

    default_aes <- aes_(x=~x, y=~y)
    mapping <- default_aes

    ly <- list(
            stat_treeScaleLine(xx=x, yy=y, width=width, color=color, offset=offset, size=linesize,
                               offset.label=offset.label, labelname=label,
                               mapping=mapping, data=data,
                               position=position, show.legend = show.legend,
                               inherit.aes = inherit.aes, na.rm=na.rm),
            stat_treeScaleText(xx=x, yy=y, width=width, color=color, offset=offset,
                               offset.label=offset.label, labelname=label,
                               size=fontsize, family = family,
                               mapping=mapping, data=data,
                               position=position, show.legend = show.legend,
                               inherit.aes = inherit.aes, na.rm=na.rm)
         )   
    if (!is.null(label)){
        ly[[3]] <- stat_treeScaleLabel(xx=x, yy=y, width=width, color=color, offset=offset,
                                offset.label=offset.label, labelname=label,
                                size=fontsize, family=family, mapping=mapping, data=data,
                                position=position, show.legend=show.legend,
                                inherit.aes = inherit.aes, na.rm=na.rm)
    }
    return(ly)
}



stat_treeScaleLine <- function(mapping=NULL, data=NULL,
                           geom="segment", position="identity",
                           xx, yy, width, offset, color, offset.label, labelname, ...,
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
                    offset.label=offset.label,
                    labelname=labelname,
                    color=color,
                    na.rm=na.rm,
                    ...)
    )
}

stat_treeScaleText <- function(mapping=NULL, data=NULL,
                               geom="text", position="identity",
                               xx, yy, width, offset, color, offset.label, labelname, ...,
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
                      offset.label=offset.label,
                      labelname=labelname,
                      color=color,
                      na.rm=na.rm,
                      vjust = 0,
                      ...)
    )
}

stat_treeScaleLabel <- function(mapping=NULL, data=NULL, 
                                geom="text", position="identity", 
                                xx, yy, width, offset, color, offset.label, labelname, ...,
                                show.legend=NA, inherit.aes=TRUE, na.rm=FALSE){
    default_aes <- aes_(x=~x, y=~y, label=~x)
    if (is.null(mapping)) {
        mapping <- default_aes
    }else{
        mapping <- modifyList(mapping, default_aes)
    }
    layer(
        stat = StatTreeScaleLabel,
        data = data,
        mapping = mapping,     
        geom = GeomText,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(xx = xx,
                     yy = yy,
                     width = width,
                     offset = offset,
                     color = color,
                     offset.label = offset.label,
                     labelname=labelname,
                     na.rm = na.rm,
                     vjust = 1,
                     ...                    
                      )
           
    )

}

StatTreeScaleLine <- ggproto("StatTreeScaleLine", Stat,
                             compute_group = function(self, data, scales, params, xx, yy, width, offset, offset.label, labelname) {
                                 get_treescale_position(data=data, xx=xx, yy=yy, width=width, 
                                                        offset=offset, offset.label=offset.label, label=labelname)[[1]]
                             },
                             required_aes = c("x", "y", "xend", "yend")
                             )


StatTreeScaleText <- ggproto("StatTreeScaleText", Stat,
                             compute_group = function(self, data, scales, params, xx, yy, width, offset, offset.label, labelname) {
                                 get_treescale_position(data=data, xx=xx, yy=yy, width=width, 
                                                        offset=offset, offset.label=offset.label, label=labelname)[[2]]
                             },
                             required_aes = c("x", "y", "label")
                             )


StatTreeScaleLabel <- ggproto("StatTreeScaleLabel", Stat, 
                              compute_panel = function(self, data, scales, params, xx, yy, width, offset, offset.label, labelname){
                                 get_treescale_position(data=data, xx=xx, yy=yy, width=width, 
                                                        offset=offset, offset.label=offset.label, label=labelname)[[3]]
                              },
                              required_aes = c("x", "y", "label")
                              )


get_treescale_position <- function(data, xx, yy, width, offset=NULL, offset.label=NULL, label=NULL) {
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
    if (is.null(offset.label)){
        offset.label <- -0.4
    } 
  
    res <-  list(LinePosition = data.frame(x=x, xend=x+d, y=y, yend=y),
                 TextPosition = data.frame(x=x+d/2, y=y+offset, label=d)
                )
  
    if (!is.null(label)){
        res[["LabelPosition"]] <- data.frame(x=x+d/2, y=y+offset.label, label=label)
    }
    
    return(res)
}
