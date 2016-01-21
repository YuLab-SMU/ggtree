##' add tree layer
##'
##' 
##' @title geom_tree
##' @param layout one of 'rectangular', 'slanted', 'circular', 'radial' or 'unrooted'
##' @param data data
##' @param mapping aes mapping
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' require(ggplot2)
##' ggplot(tr) + geom_tree()
geom_tree <- function(layout="rectangular", data=NULL, mapping=NULL, ...) {
    position <- "identity"
    show.legend <- NA
    na.rm <- FALSE
    inherit.aes <- FALSE

    stat_tree(layout=layout, data=data, mapping=mapping, geom="segment",
              position=position, show.legend=show.legend,
              inherit.aes=inherit.aes, na.rm=na.rm, ...)

}


stat_tree <- function(mapping=NULL, data=NULL, geom="segment", position="identity",
                      layout, ...,
                      show.legend=NA, inherit.aes=FALSE, na.rm=FALSE) {
    default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    if (layout %in% c("rectangular", "fan", "circular")) {
        list(layer(stat=StatTreeHorizontal,
                   data=data,
                   mapping=mapping,
                   geom = geom,
                   position=position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params=list(layout=layout,
                               na.rm=na.rm,
                          ...)
                   ),
             layer(stat=StatTreeVertical,
                   data=data,
                   mapping=mapping,
                   geom = geom,
                   position=position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params=list(layout=layout,
                               na.rm=na.rm,
                               ...)
                   )
             )
    } else if (layout %in% c("slanted", "radial", "unrooted")) {
        layer(stat=StatTree,
              data=data,
              mapping=mapping,
              geom = geom,
              position=position,
              show.legend = show.legend,
              inherit.aes = inherit.aes,
              params=list(layout=layout,
                          na.rm=na.rm,
                          ...)
              )
    }    
}

StatTreeHorizontal <- ggproto("StatTreeHorizontal", Stat,
                    compute_group = function(self, data, scales, params, layout) {
                        df <- setup_tree_data(data)
                        x <- df$x
                        y <- df$y
                        parent <- df$parent
                        data.frame(x = x[parent],
                                   y = y,
                                   xend = x,
                                   yend = y)
                    },
                    required_aes = c("x", "y", "xend", "yend")
                    )

StatTreeVertical <- ggproto("StatTreeVertical", Stat,
                    compute_group = function(self, data, scales, params, layout) {
                        df <- setup_tree_data(data)
                        x <- df$x
                        y <- df$y
                        parent <- df$parent

                        data.frame(x = x[parent],
                                   y = y[parent], 
                                   xend = x[parent],
                                   yend = y)
                    },
                    required_aes = c("x", "y", "xend", "yend")
                    )



StatTree <- ggproto("StatTree", Stat,
                    compute_group = function(self, data, scales, params, layout) {
                        df <- setup_tree_data(data)
                        x <- df$x
                        y <- df$y
                        parent <- df$parent
                        data.frame(x = x[parent],
                                   y = y[parent],
                                   xend = x,
                                   yend = y)
                        
                    },
                    required_aes = c("x", "y", "xend", "yend")
                    )


setup_tree_data <- function(data) {
    data <- data[order(data$node, decreasing = FALSE), ]
    data[match(unique(data$node), data$node),]
}


##' add tree layer
##'
##' 
##' @title geom_tree2
##' @param layout one of 'rectangular', 'slanted', 'circular', 'radial' or 'unrooted'
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
geom_tree2 <- function(layout="rectangular", ...) {
    x <- y <- parent <- NULL
    lineend  = "round"
    if (layout == "rectangular" || layout == "fan" || layout == "circular") {
        list(
            geom_segment(aes(x    = x[parent],
                             xend = x,
                             y    = y,
                             yend = y),
                         lineend  = lineend, ...),
            
            geom_segment(aes(x    = x[parent],
                             xend = x[parent],
                             y    = y[parent],
                             yend = y),
                         lineend  = lineend, ...)
            )
    } else if (layout == "slanted" || layout == "radial" || layout == "unrooted") {
        geom_segment(aes(x    = x[parent],
                         xend = x,
                         y    = y[parent],
                         yend = y),
                     lineend  = lineend, ...)
    }
}

