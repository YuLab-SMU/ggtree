##' annotate a clade with bar and text label
##'
##'
##' @title geom_cladelabel
##' @param node selected node
##' @param label clade label
##' @param offset offset of bar and text from the clade
##' @param offset.text offset of text from bar
##' @param extend extend bar height
##' @param align logical
##' @param barsize size of bar
##' @param fontsize size of text
##' @param angle angle of text
##' @param geom one of 'text' or 'label'
##' @param hjust justify text horizontally
##' @param color color for clade & label, of length 1 or 2
##' @param fill fill label background, only work with geom='label'
##' @param family sans by default, can be any supported font
##' @param parse logical, whether parse label
##' @param ... additional parameter
##' @return ggplot layers
##' @export
##' @author Guangchuang Yu
##' @seealso [geom_cladelabel2]
geom_cladelabel <- function(node, label,
                            offset      = 0,
                            offset.text = 0,
                            extend      = 0,
                            align       = FALSE,
                            barsize     = 0.5,
                            fontsize    = 3.88,
                            angle       = 0,
                            geom        = "text",
                            hjust       = 0,
                            color       = NULL,
                            fill        = NA,
                            family      = "sans",
                            parse       = FALSE,
                            ...) {

    structure(list(node = node,
                   label = label,
                   offset = offset,
                   offset.text = offset.text,
                   extend = extend,
                   align =  align,
                   barsize = barsize,
                   fontsize = fontsize,
                   angle = angle,
                   geom = geom,
                   hjust = hjust,
                   color = color,
                   fill = fill,
                   family = family,
                   parse = parse,
                   ...),
              class = 'cladelabel')
}


geom_cladelabel_rectangular <- function(node, label,
                            offset      = 0,
                            offset.text = 0,
                            extend      = 0,
                            align       = FALSE,
                            barsize     = 0.5,
                            fontsize    = 3.88,
                            angle       = 0,
                            geom        = "text",
                            hjust       = 0,
                            color       = NULL,
                            fill        = NA,
                            family      = "sans",
                            parse       = FALSE,
                            ...) {
    mapping <- NULL
    data <- NULL
    position <- "identity"
    show.legend <- NA
    na.rm <- TRUE
    inherit.aes <- FALSE

    if (!is.null(color)) {
        if (length(color) > 2) {
            stop("color should be of length 1 or 2")
        }
        if (length(color) == 0) {
            color = NULL
        } else if (length(color) == 1) {
            barcolor <- color
            labelcolor <- color
        } else {
            barcolor <- color[1]
            labelcolor <- color[2]
        }
    }

    if (parse == 'emoji') {
        emoji <- get_fun_from_pkg("emojifont", "emoji")
        label <- emoji(label)
        parse <- FALSE
        family <- "EmojiOne"
    }

    if (is.null(color)) {
        if (geom == "text") {
            ## no fill parameter
            layer_text = stat_cladeText(node=node, label=label, offset=offset+offset.text,
                                        align=align, size=fontsize, angle=angle, family=family,
                                        mapping=mapping, data=data, geom=geom, hjust=hjust,
                                        position=position, show.legend = show.legend,
                                        inherit.aes = inherit.aes, na.rm=na.rm, parse=parse, ...)

        } else {
            layer_text = stat_cladeText(node=node, label=label, offset=offset+offset.text,
                                        align=align, size=fontsize, angle=angle, fill=fill,family=family,
                                        mapping=mapping, data=data, geom=geom, hjust=hjust,
                                        position=position, show.legend = show.legend,
                                        inherit.aes = inherit.aes, na.rm=na.rm,
                                        parse = parse,  ...)
        }

        layer_bar <- stat_cladeBar(node=node, offset=offset, align=align,
                                   size=barsize, extend = extend,
                                   mapping=mapping, data=data,
                                   position=position, show.legend = show.legend,
                                   inherit.aes = inherit.aes, na.rm=na.rm, ...)
    } else {
        if (geom == "text") {
            ## no fill parameter
            layer_text = stat_cladeText(node=node, label=label, offset=offset+offset.text,
                                        align=align, size=fontsize, angle=angle, color=labelcolor, family=family,
                                        mapping=mapping, data=data, geom=geom, hjust=hjust,
                                        position=position, show.legend = show.legend,
                                        inherit.aes = inherit.aes, na.rm=na.rm, parse=parse,  ...)

        } else {
            layer_text = stat_cladeText(node=node, label=label, offset=offset+offset.text,
                                        align=align, size=fontsize, angle=angle, color=labelcolor,
                                        fill=fill,family=family,
                                        mapping=mapping, data=data, geom=geom, hjust=hjust,
                                        position=position, show.legend = show.legend,
                                        inherit.aes = inherit.aes, na.rm=na.rm,
                                        parse = parse,  ...)
        }

        layer_bar <- stat_cladeBar(node        = node,
                                   offset      = offset,
                                   align       = align,
                                   size        = barsize,
                                   color       = barcolor,
                                   extend      = extend,
                                   mapping     = mapping,
                                   data        = data,
                                   position    = position,
                                   show.legend = show.legend,
                                   inherit.aes = inherit.aes,
                                   na.rm       = na.rm, ...)


    }

    list(
      layer_bar,
      layer_text
    )
}


stat_cladeText <- function(mapping = NULL, data = NULL,
                           geom = "text", position = "identity",
                           node, label, offset, align, ..., angle,
                           show.legend = NA, inherit.aes = FALSE,
                           na.rm = FALSE, parse = FALSE) {

    default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, angle=~angle)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    layer(stat = StatCladeText,
          data = data,
          mapping = mapping,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params=list(node   = node,
                      label  = label,
                      offset = offset,
                      align  = align,
                      na.rm  = na.rm,
                      parse  = parse,
                      angle_ = angle,
                      ...),
          check.aes = FALSE
          )


}

stat_cladeBar <- function(mapping=NULL, data=NULL,
                          geom="segment", position="identity",
                          node, offset, align, extend,  ...,
                          show.legend=NA, inherit.aes=FALSE, na.rm=FALSE) {


    default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, xend=~x, yend=~y)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    layer(stat = StatCladeBar,
          data = data,
          mapping = mapping,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(node = node,
                      offset = offset,
                      extend = extend,
                      align  = align,
                      na.rm  = na.rm,
                      ...),
          check.aes = FALSE
          )
}

StatCladeText <- ggproto("StatCladeText", Stat,
                         compute_group = function(self, data, scales, params, node, label, offset, align, angle_) {
                             df <- get_cladelabel_position(data, node, offset, align, adjustRatio = 1.03, angle_)
                             df$y <- mean(c(df$y, df$yend))
                             df$label <- label
                             return(df)
                         },
                         required_aes = c("x", "y", "label", "angle")
                         )

StatCladeBar <- ggproto("StatCladBar", Stat,
                        compute_group = function(self, data, scales, params, node, offset, align, extend) {
                            get_cladelabel_position(data, node, offset, align, adjustRatio=1.02,
                                                    angle=0, extend=extend)
                        },
                        required_aes = c("x", "y", "xend", "yend")
)


get_cladelabel_position <- function(data, node, offset, align,
                                    adjustRatio, angle="auto", extend=0) {
    df <- get_cladelabel_position_(data, node, angle, extend)
    if (align) {
        # Find max x value for all tree nodes so all clade labels align to same position.
        mx <- max(data$x, na.rm=TRUE)
    } else {
        mx <- df$x
    }

    angle <- df$angle
    ## if (angle >= 90 & angle <=270) {
    ##     angle <- angle + 180
    ## }

    mx <- mx * adjustRatio + offset

    data.frame(x=mx, xend=mx, y=df$y, yend=df$yend, angle=angle)
}

# get x, y and yend of clade region.
get_cladelabel_position_ <- function(data, node, angle = "auto", extend = 0) {
    if (length(extend) == 1) {
        extend = rep(extend, 2)
    }

    ## sp <- get.offspring.df(data, node)
    ## sp2 <- c(sp, node)
    ## sp.df <- data[match(sp2, data$node),]

    sp.df <- offspring.tbl_tree(data, node, self_include = TRUE)

    y <- sp.df$y
    y <- y[!is.na(y)]
    mx <- max(sp.df$x, na.rm=TRUE)

    d <- data.frame(x=mx, y=min(y) - extend[2], yend=max(y) + extend[1])
    if (missing(angle))
        return(d)

    if (angle == "auto") {
        d$angle <- mean(range(sp.df$angle))
    } else {
        d$angle <- angle
    }

    return(d)

}

