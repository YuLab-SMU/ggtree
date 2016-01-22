##' add tree layer
##'
##' 
##' @title geom_tree2
##' @param mapping aesthetic mapping
##' @param data data
##' @param layout one of 'rectangular', 'slanted', 'circular', 'radial' or 'unrooted'
##' @param setup_data logical
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
geom_tree2 <- function(mapping=NULL, data=NULL, layout="rectangular", setup_data=FALSE, ...) {
    stat_tree(data=data, mapping=mapping, geom="segment",
              layout=layout, setup_data=setup_data, lineend="round", 
              position='identity', show.legend=NA,
              inherit.aes=TRUE, na.rm=TRUE, ...)
}


stat_tree <- function(mapping=NULL, data=NULL, geom="segment", position="identity",
                      layout="rectangular", setup_data=FALSE, lineend="round", ...,
                      show.legend=NA, inherit.aes=TRUE, na.rm=FALSE) {
    
    default_aes <- aes_(x=~x, y=~y,node=~node, parent=~parent)
    if (setup_data) {
        default_aes <- modifyList(default_aes, aes_(.id=~.id))
    }
    
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    if (layout %in% c("rectangular", "fan", "circular")) {
        list(layer(data=data,
                   mapping=mapping,
                   stat=StatTreeHorizontal,
                   geom = geom,
                   position=position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params=list(layout = layout,
                               setup_data = setup_data,
                               lineend = lineend,
                               na.rm = na.rm,
                          ...)
                   ),
             layer(data=data,
                   mapping=mapping,
                   stat=StatTreeVertical,
                   geom = geom,
                   position=position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params=list(layout = layout,
                               setup_data = setup_data,
                               lineend = lineend,
                               na.rm = na.rm,
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
              params=list(layout = layout,
                          setup_data = setup_data,
                          lineend = lineend,
                          na.rm = na.rm,
                          ...)
              )
    }    
}

StatTreeHorizontal <- ggproto("StatTreeHorizontal", Stat,
                              required_aes = c("node", "parent", "x", "y"),
                              compute_panel = function(self, data, scales, params, layout, setup_data, lineend) {
                                  df <- setup_tree_data(data, setup_data)
                                  x <- df$x
                                  y <- df$y
                                  parent <- df$parent
                                  df$xend <- x
                                  df$yend <- y
                                  df$x <- x[parent]
                                  return(df)
                              }
                              )

StatTreeVertical <- ggproto("StatTreeVertical", Stat,
                            required_aes = c("node", "parent", "x", "y"),
                            compute_panel = function(self, data, scales, params, layout, setup_data, lineend) {                                
                                df <- setup_tree_data(data, setup_data)
                                x <- df$x
                                y <- df$y
                                parent <- df$parent
                                
                                ## data.frame(x = x[parent],
                                ##            y = y[parent], 
                                ##            xend = x[parent],
                                ##            yend = y)

                                df$x <- x[parent]
                                df$y <- y[parent]
                                df$xend <- x[parent]
                                df$yend <- y
                                return(df)
                            }
                            )



StatTree <- ggproto("StatTree", Stat,
                    required_aes = c("node", "parent", "x", "y"),
                    compute_panel = function(self, data, scales, params, layout, setup_data, lineend) {
                        df <- setup_tree_data(data, setup_data)
                        x <- df$x
                        y <- df$y
                        parent <- df$parent
                        df$x <- x[parent]
                        df$y <- y[parent]
                        df$xend <- x
                        df$yend <- y
                        return(df)                        
                    }
                    )


setup_tree_data <- function(data, setup_data=FALSE) {
    if (!setup_data)
        return(data)
    if (nrow(data) == length(unique(data$node)))
        return(data)
    
    data[match(unique(data$node), data$node),]
    # data[order(data$node, decreasing = FALSE), ]
}


##' add tree layer
##'
##' 
##' @title geom_tree
##' @param layout one of 'rectangular', 'slanted', 'circular', 'radial' or 'unrooted'
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
geom_tree <- function(layout="rectangular", ...) {
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

