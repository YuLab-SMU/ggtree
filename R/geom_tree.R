##' add tree layer
##'
##'
##' @title geom_tree
##' @param mapping aesthetic mapping
##' @param data data
##' @param layout one of 'rectangular', 'slanted', 'circular', 'radial' or 'unrooted'
##' @param multiPhylo logical
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
geom_tree <- function(mapping=NULL, data=NULL, layout="rectangular", multiPhylo=FALSE, ...) {
    stat_tree(data=data, mapping=mapping, geom="segment",
              layout=layout, multiPhylo=multiPhylo, lineend="round",
              position='identity', show.legend=NA,
              inherit.aes=TRUE, na.rm=TRUE, ...)
}


stat_tree <- function(mapping=NULL, data=NULL, geom="segment", position="identity",
                      layout="rectangular", multiPhylo=FALSE, lineend="round", ...,
                      show.legend=NA, inherit.aes=TRUE, na.rm=FALSE) {

    default_aes <- aes_(x=~x, y=~y,node=~node, parent=~parent)
    if (multiPhylo) {
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
                               lineend = lineend,
                               na.rm = na.rm,
                               ...),
                   if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
                   ),
             layer(data=data,
                   mapping=mapping,
                   stat=StatTreeVertical,
                   geom = geom,
                   position=position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params=list(layout = layout,
                               lineend = lineend,
                               na.rm = na.rm,
                               ...),
                   if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
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
                          lineend = lineend,
                          na.rm = na.rm,
                          ...),
              if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
              )
    }
}

StatTreeHorizontal <- ggproto("StatTreeHorizontal", Stat,
                              required_aes = c("node", "parent", "x", "y"),
                              compute_group = function(data, params) {
                                  data
                              },
                              compute_panel = function(self, data, scales, params, layout, lineend) {
                                  .fun <- function(data) {
                                      df <- setup_tree_data(data)
                                      x <- df$x
                                      y <- df$y
                                      df$xend <- x
                                      df$yend <- y
                                      ii <- with(df, match(parent, node))
                                      df$x <- x[ii]
                                      return(df)
                                  }

                                  if ('.id' %in% names(data)) {
                                      ldf <- split(data, data$.id)
                                      df <- do.call(rbind, lapply(ldf, .fun))
                                  } else {
                                      df <- .fun(data)
                                  }
                                  return(df)
                              }
                              )

StatTreeVertical <- ggproto("StatTreeVertical", Stat,
                            required_aes = c("node", "parent", "x", "y"),
                            compute_group = function(data, params) {
                                data
                            },
                            compute_panel = function(self, data, scales, params, layout, lineend) {
                                .fun <- function(data) {
                                    df <- setup_tree_data(data)
                                    x <- df$x
                                    y <- df$y
                                    ii <- with(df, match(parent, node))
                                    df$x <- x[ii]
                                    df$y <- y[ii]
                                    df$xend <- x[ii]
                                    df$yend <- y
                                    return(df)
                                }
                                if ('.id' %in% names(data)) {
                                    ldf <- split(data, data$.id)
                                    df <- do.call(rbind, lapply(ldf, .fun))
                                } else {
                                    df <- .fun(data)
                                }
                                return(df)
                            }
                            )



StatTree <- ggproto("StatTree", Stat,
                    required_aes = c("node", "parent", "x", "y"),
                    compute_group = function(data, params) {
                        data
                    },
                    compute_panel = function(self, data, scales, params, layout, lineend) {
                        .fun <- function(data) {
                            df <- setup_tree_data(data)
                            x <- df$x
                            y <- df$y
                            ii <- with(df, match(parent, node))
                            df$x <- x[ii]
                            df$y <- y[ii]
                            df$xend <- x
                            df$yend <- y
                            return(df)
                        }
                        if ('.id' %in% names(data)) {
                            ldf <- split(data, data$.id)
                            df <- do.call(rbind, lapply(ldf, .fun))
                        } else {
                            df <- .fun(data)
                        }
                        return(df)
                    }
                    )


setup_tree_data <- function(data) {
    if (nrow(data) == length(unique(data$node)))
        return(data)

    data[match(unique(data$node), data$node),]
    ## data[order(data$node, decreasing = FALSE), ]
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

