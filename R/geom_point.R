
##' add tip point
##'
##'
##' @title geom_tippoint
##' @inheritParams geom_point2
##' @return tip point layer
##' @export
##' @author Guangchuang Yu
geom_tippoint <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    isTip <- NULL
    self_mapping <- aes(subset = isTip)
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        mapping <- modifyList(self_mapping, mapping)
    }
    geom_point2(mapping, data, position, na.rm, show.legend, inherit.aes, ...)
}

## angle is not supported,
## https://github.com/GuangchuangYu/ggtree/issues/77
##
##
## geom_tippoint2 <- function(mapping=NULL, hjust=0, ...) {
##     angle <- NULL
##     isTip <- NULL
##     m1 <- aes(subset=(isTip & (angle < 90 | angle > 270)), angle=angle)
##     m2 <- aes(subset=(isTip & (angle >= 90 & angle <=270)), angle=angle+180)

##     if (!is.null(mapping)) {
##         m1 <- modifyList(mapping, m1)
##         m2 <- modifyList(mapping, m2)
##     }

##     list(geom_tippoint(m1, hjust=hjust, ...),
##          geom_tippoint(m2, hjust=1-hjust, ...)
##          )
## }


##' add node point
##'
##'
##' @title geom_nodepoint
##' @inheritParams geom_point2
##' @return node point layer
##' @export
##' @author Guangchuang Yu
geom_nodepoint <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
    isTip <- NULL
    self_mapping <- aes(subset = !isTip)
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        mapping %<>% modifyList(self_mapping)
    }
    geom_point2(mapping, data, position, na.rm, show.legend, inherit.aes, ...)
}


##' add root point
##'
##'
##' @title geom_rootpoint
##' @inheritParams geom_point2
##' @return root point layer
##' @export
##' @author Guangchuang Yu
geom_rootpoint <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
    isTip <- node <- parent <- NULL
    self_mapping <- aes(subset = (node == parent))
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        mapping %<>% modifyList(self_mapping)
    }
    geom_point2(mapping, data, position, na.rm, show.legend, inherit.aes, ...)
}


##' geom_point2 support aes(subset) via setup_data
##'
##'
##' @title geom_point2
##' @param mapping aes mapping
##' @param data data
##' @param position position
##' @param na.rm logical
##' @param show.legend logical
##' @param inherit.aes logical
##' @param ... addktional parameter
##' @importFrom ggplot2 layer
##' @export
##' @seealso
##' \link[ggplot2]{geom_point}
##' @return point layer
##' @author Guangchuang Yu
geom_point2 <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {


    default_aes <- aes_(node=~node)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    layer(
        data = data,
        mapping = mapping,
        stat = StatTreeData,
        geom = GeomPointGGtree,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        ),
        if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
    )
}

##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 GeomPoint
##' @importFrom ggplot2 draw_key_point
GeomPointGGtree <- ggproto("GeomPointGGtree", GeomPoint,
                           setup_data = function(data, params) {
                               if (is.null(data$subset))
                                   return(data)
                               data[data$subset,]
                           }

                           ## ,

                           ## draw_panel = function(data, panel_scales, coord, na.rm = FALSE){
                           ##     GeomPoint$draw_panel(data, panel_scales, coord, na.rm)
                           ## },

                           ## draw_key = draw_key_point,

                           ## required_aes = c("x", "y"),
                           ## default_aes = aes(shape = 19, colour = "black", size = 1.5, fill = NA,
                           ##                   alpha = NA, stroke = 0.5)
                            )

