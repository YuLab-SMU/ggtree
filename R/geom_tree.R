##' add tree layer
##'
##'
##' @title geom_tree
##' @param mapping aesthetic mapping
##' @param data data
##' @param layout one of 'rectangular', 'dendrogram', 'slanted', 'ellipse', 'roundrect',
##' 'fan', 'circular', 'inward_circular', 'radial', 'equal_angle', 'daylight' or 'ape'
##' @param multiPhylo logical, whether input data contains multiple phylo class.
##' @param ... additional parameter
##' 
##' some dot arguments:
##' \itemize{
##'    \item \code{continuous} character, continuous transition for selected aesthethic ('size' or 'color'('colour')). It 
##'     should be one of 'color' (or 'colour'), 'size', 'all' and 'none', default is 'none'.
##'    \item \code{nsplit} integer, the number of branch blocks divided when 'continuous' is not "none", default is 200.
##' }
##' @return tree layer
##' @section Aesthetics:
#' \code{geom_tree()} understands the following aesthethics:
##'     \itemize{
##'        \item \code{color} character, control the color of line, default is black (\code{continuous} is "none").
##'        \item \code{linetype} control the type of line, default is 1 (solid).
##'        \item \code{size} numeric, control the width of line, default is 0.5 (\code{continuous} is "none").
##'     }
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
geom_tree <- function(mapping=NULL, data=NULL, layout="rectangular", multiPhylo=FALSE, ...) {
    stat_tree(data=data, mapping=mapping, geom="segment",
              layout=layout, multiPhylo=multiPhylo, ...)
}


stat_tree <- function(mapping=NULL, data=NULL, geom="segment", position="identity",
                      layout="rectangular", multiPhylo=FALSE, lineend="round", MAX_COUNT=5,
                      ..., arrow=NULL, rootnode=TRUE, show.legend=NA, inherit.aes=TRUE,
                      na.rm=TRUE, check.param=TRUE) {

    default_aes <- aes_(x=~x, y=~y,node=~node, parent=~parent)
    if (multiPhylo) {
        default_aes <- modifyList(default_aes, aes_(.id=~.id))
    }

    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(default_aes, mapping)
    }

    if (!is.null(arrow)) {
        rootnode <- FALSE
    }

    if (layout %in% c("rectangular", "dendrogram", "fan", "circular", "inward_circular")) {
        list(             
             layer(data=data,
                   mapping=mapping,
                   stat=StatTreeHorizontal,
                   geom = geom, ## GeomTreeHorizontal,
                   position=position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params=list(layout = layout,
                               lineend = lineend,
                               na.rm = na.rm,
                               arrow = arrow,
                               rootnode = rootnode,
                               ...),
                   check.aes = FALSE
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
                               ## arrow = arrow,
                               rootnode = rootnode,
                               ...),
                   check.aes = FALSE
                   )
             )
    } else if (layout %in% c("slanted", "radial", "equal_angle", "daylight", "ape")) {
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
                          arrow = arrow,
                          rootnode = rootnode,
                          ...),
              check.aes = FALSE
              )
    } else if (layout %in% c("ellipse", "roundrect")){
        mapping <- modifyList(mapping, aes_(isTip=~isTip))
        layer(stat=StatTreeEllipse,
              data=data,
              mapping=mapping,
              geom=GeomCurvelink,
              position=position,
              show.legend=show.legend,
              inherit.aes=inherit.aes,
              params=list(layout=layout,
                          lineend = lineend,
                          na.rm = na.rm,
                          arrow = arrow,
                          rootnode = rootnode,
                          ...),
              check.aes=FALSE
              )
    }
}

## GeomTreeHorizontal <- ggproto("GeomTreeHorizontal",  GeomSegment,
##                               draw_panel =  function(data, panel_params, coord, ...) {
##                                   coords <- coord$transform(data, panel_params)
##                                   GeomSegment$draw_panel(data = data, panel_params = panel_params,
##                                                          coord = coord, ...)
##                               }
##                               )

StatTreeHorizontal <- ggproto("StatTreeHorizontal", Stat,
                              required_aes = c("node", "parent", "x", "y"),
                              compute_group = function(data, params) {
                                data
                              },
                              compute_panel = function(self, data, scales, params, layout, lineend,
                                                       continuous = "none", rootnode = TRUE, 
                                                       nsplit = 100, extend=0.002 ) {
                                  .fun <- function(data) {
                                      df <- setup_tree_data(data)
                                      x <- df$x
                                      y <- df$y
                                      df$xend <- x
                                      df$yend <- y
                                      ii <- with(df, match(parent, node))
                                      df$x <- x[ii]

                                      if (!rootnode) {
                                          ## introduce this paramete in v=1.7.4
                                          ## rootnode = TRUE which behave as previous versions.
                                          ## and has advantage of the number of line segments is consistent with tree nodes.
                                          ## i.e. every node has its own line segment, even for root.
                                          ## if rootnode = FALSE, the root to itself line segment will be removed.

                                          df <- dplyr::filter(df, .data$node != tidytree:::rootnode.tbl_tree(df)$node)
                                      }
                                      if (is.logical(continuous)){
                                          warning_wrap('The continuous argument type was changed (v>=2.5.2). Now, it should be one of "color"(or "colour"), "size", "all", and "none".')
                                          ifelse(continuous, 
                                                 warning_wrap('It was set to TRUE, it should be replaced with "color"(or "colour"), this meaning the aesthethic of "color"(or "colour") is continuous.'),
                                                 warning_wrap('It was set to FALSE, it should be replaced with "none", this meaning the aesthethic of "color"(or "colour") or "size" will not be continuous.')
                                          )
                                          continuous <- ifelse(continuous, "color", "none")
                                      }
                                      continuous <- match.arg(continuous, c("color", "colour", "size", "none", "all"))
                                      if (continuous != "none") {
                                          # using ggnewscale new_scale("color") for multiple color scales
                                          if (length(grep("colour_new", names(df)))==1 && !"colour" %in% names(df)){
                                              names(df)[grep("colour_new", names(df))] <- "colour"
                                          }
                                          if (!is.null(df$colour)){
                                              if (any(is.na(df$colour))){
                                                  df$colour[is.na(df$colour)] <- 0
                                              }
                                              df$col2 <- df$colour
                                              df$col <- df$col2[ii]
                                          }
                                          # using ggnewscale new_scale("size") for multiple size scales
                                          if (length(grep("size_new", names(df)))==1 && !"size" %in% names(df)){
                                              names(df)[grep("size_new", names(df))] <- "size"
                                          }
                                          if (!is.null(df$size)){
                                              if (any(is.na(df$size))){
                                                  df$size[is.na(df$size)] <- 0
                                              }
                                              df$size2 <- df$size
                                              df$size1 <- df$size2[ii]
                                          }
                                          setup_data_continuous_color_size_tree(df, nsplit = nsplit, extend = extend, continuous = continuous)
                                      } else {
                                          return(df)
                                      }
                                  }
                                  if ('.id' %in% names(data)) {
                                      ldf <- split(data, data$.id)
                                      df <- do.call(rbind, lapply(ldf, .fun))
                                  } else {
                                      df <- .fun(data)
                                  }
                                  # using ggnewscale new_scale for multiple color or size scales
                                  if (length(grep("colour_new", names(data)))==1 && continuous != "none"){
                                      names(df)[match("colour", names(df))] <- names(data)[grep("colour_new", names(data))] 
                                  }
                                  if (length(grep("size_new", names(data)))==1 && continuous != "none"){
                                      names(df)[match("size", names(df))] <- names(data)[grep("size_new", names(data))]
                                  }
                                  return(df)
                              }
                              )


StatTreeVertical <- ggproto("StatTreeVertical", Stat,
                            required_aes = c("node", "parent", "x", "y"),
                            compute_group = function(data, params) {
                                data
                            },
                            compute_panel = function(self, data, scales, params, layout, lineend,
                                                     continuous = "none", nsplit=100, 
                                                     extend=0.002, rootnode = TRUE) {
                                .fun <- function(data) {
                                    df <- setup_tree_data(data)
                                    x <- df$x
                                    y <- df$y
                                    ii <- with(df, match(parent, node))
                                    df$x <- x[ii]
                                    df$y <- y[ii]
                                    df$xend <- x[ii]
                                    df$yend <- y

                                    if (!rootnode) {
                                        df <- dplyr::filter(df, .data$node != rootnode.tbl_tree(df)$node)
                                    }

                                    if (is.logical(continuous)){
                                        continuous <- ifelse(continuous, "color", "none")
                                    }

                                    if (continuous != "none"){
                                        # using ggnewscale new_scale("color") for multiple color scales
                                        if (length(grep("colour_new", names(df)))==1 && !"colour" %in% names(df)){
                                            names(df)[grep("colour_new", names(df))] <- "colour"
                                        }
                                        if (!is.null(df$colour)){
                                            if (any(is.na(df$colour))){
                                                df$colour[is.na(df$colour)] <- 0
                                            }
                                            df$colour <- df$colour[ii]
                                        }
                                        # using ggnewscale new_scale("size") for multiple size scales
                                        if (length(grep("size_new", names(df)))==1 && !"size" %in% names(df)){
                                            names(df)[grep("size_new", names(df))] <- "size"
                                        }
                                        if (!is.null(df$size)){
                                            if (any(is.na(df$size))){
                                                df$size[is.na(df$size)] <- 0
                                            }
                                            df$size <- df$size[ii]
                                        }
                                    }
                                    return(df)
                                }
                                
                                if ('.id' %in% names(data)) {
                                    ldf <- split(data, data$.id)
                                    df <- do.call(rbind, lapply(ldf, .fun))
                                } else {
                                    df <- .fun(data)
                                }
                                
                                # using ggnewscale new_scale for multiple color or size scales
                                if (length(grep("colour_new", names(data)))==1 && continuous != "none"){
                                    names(df)[match("colour", names(df))] <- names(data)[grep("colour_new", names(data))]
                                }
                                if (length(grep("size_new", names(data)))==1 && continuous != "none"){
                                    names(df)[match("size", names(df))] <- names(data)[grep("size_new", names(data))]
                                }
                                return(df)
                            }
                            )



StatTree <- ggproto("StatTree", Stat,
                    required_aes = c("node", "parent", "x", "y"),
                    compute_group = function(data, params) {
                        data
                    },
                    compute_panel = function(self, data, scales, params, layout, lineend,
                                             continuous =  "none", nsplit = 100, 
                                             extend = 0.002, rootnode = TRUE) {
                        .fun <- function(data) {
                            df <- setup_tree_data(data)
                            x <- df$x
                            y <- df$y
                            ii <- with(df, match(parent, node))
                            df$x <- x[ii]
                            df$y <- y[ii]
                            df$xend <- x
                            df$yend <- y

                            if (!rootnode) {
                                df <- dplyr::filter(df, .data$node != rootnode.tbl_tree(df)$node)
                            }
                            if (is.logical(continuous)){
                                warning_wrap('The continuous argument type was changed (v>=2.5.2). Now, it should be one of "color" (or "colour"), "size", "all", and "none".')
                                ifelse(continuous, 
                                       warning_wrap('It was set to TRUE, it should be replaced with "color" (or "colour"), this meaning the aesthethic of "color" (or "colour") is continuous.'),
                                       warning_wrap('It was set to FALSE, it should be replaced with "none", this meaning the aesthethic of "color" (or "colour") or "size" will not be continuous.')
                                )
                                continuous <- ifelse(continuous, "color", "none")
                            }
                            continuous <- match.arg(continuous, c("color", "colour", "size", "none", "all"))
                            if (continuous != "none") {
                                # using ggnewscale new_scale("color") for multiple color scales
                                if (length(grep("colour_new", names(df)))==1 && !"colour" %in% names(df)){
                                    names(df)[grep("colour_new", names(df))] <- "colour"
                                }
                                if (!is.null(df$colour)){
                                    if (any(is.na(df$colour))){
                                        df$colour[is.na(df$colour)] <- 0
                                    }
                                    df$col2 <- df$colour
                                    df$col <- df$col2[ii]
                                }
                                # using ggnewscale new_scale("size") for multiple size scales
                                if (length(grep("size_new", names(df)))==1 && !"size" %in% names(df)){
                                    names(df)[grep("size_new", names(df))] <- "size"
                                }
                                if (!is.null(df$size)){
                                    if (any(is.na(df$size))){
                                        df$size[is.na(df$size)] <- 0
                                    }
                                    df$size2 <- df$size
                                    df$size1 <- df$size2[ii]
                                }
                                setup_data_continuous_color_size_tree(df, nsplit = nsplit, extend = extend, continuous = continuous)
                            } else{
                                return(df)
                            }
                        }
                        if ('.id' %in% names(data)) {
                            ldf <- split(data, data$.id)
                            df <- do.call(rbind, lapply(ldf, .fun))
                        } else {
                            df <- .fun(data)
                        }
                        
                        # using ggnewscale new_scale for multiple color or size scales
                        if (length(grep("colour_new", names(data)))==1 && continuous != "none"){
                            names(df)[match("colour", names(df))] <- names(data)[grep("colour_new", names(data))]
                        }
                        if (length(grep("size_new", names(data)))==1 && continuous != "none"){
                            names(df)[match("size", names(df))] <- names(data)[grep("size_new", names(data))]
                        }

                        return(df)
                    }
                    )


StatTreeEllipse <- ggproto("StatTreeEllipse", Stat,
                           required_aes = c("node", "parent", "x", "y", "isTip"),
                           compute_group = function(data, params){
                               data
                           },
                           compute_panel = function(self, data, scales, params, layout, lineend, 
                                                    continuous = "none", nsplit = 100, 
                                                    extend = 0.002, rootnode = TRUE){
                               if (continuous !="none" || continuous){
                                   stop("continuous colour or size are not implemented for roundrect or ellipse layout")
                               }
                               df <- StatTree$compute_panel(data = data, scales = scales, 
                                                            params = params, layout = layout, lineend = lineend,
                                                            continuous = continuous, nsplit = nsplit, 
                                                            extend = extend, rootnode = rootnode)
                               df <- df[!(df$x==df$xend & df$y==df$yend),]
                               reverseflag <- check_reverse(df)
                               if (layout=="ellipse"){
                                   if (reverseflag){
                                       df$curvature <- ifelse(df$y > df$yend, -1, 1) * 0.5
                                   }else{
                                       df$curvature <- ifelse(df$y > df$yend, 1, -1) * 0.5
                                   }
                                   df$curveangle <- ifelse(df$y > df$yend, 20, 160)
                               }else if (layout=="roundrect"){
                                   if (reverseflag){
                                       df$curvature <- ifelse(df$y > df$yend, -1, 1)
                                   }else{
                                       df$curvature <- ifelse(df$y > df$yend, 1, -1)
                                   }
                                   df$curveangle <- 90
                               }
                               df$square <- TRUE 
                               return (df)
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

setup_data_continuous_color_size <- function(x, xend, y, yend, col, col2, size1, size2,
                                        xrange = NULL, nsplit = 100, extend = 0.002) {
    if (is.null(xrange))
        xrange <- c(x, xend)

    ## xstep <- diff(xrange)/nsplit
    ## xn <- floor((xend - x)/xstep)
    xn <- floor((xend - x) * nsplit /diff(xrange))
    ## slope <- (yend - y)/(xend - x)
    ydiff <- yend - y
    xdiff <- xend - x

    if (xn > 0) {
        ## x <- x + 0:xn * xstep
        x <- x + 0:xn * diff(xrange) / nsplit 
        tmp <- x[-1] * (1 + extend)
        tmp[tmp > xend] <- xend
        xend <- c(tmp, xend)
        ## y <- y + 0:xn * xstep * slope
        y <- y + 0:xn * diff(xrange) * ydiff / (nsplit * xdiff)
        ## yend <- y + (xend - x) * slope
        yend <- y + (xend - x) * ydiff / xdiff 
    }

    n <- length(x)
    if (is.numeric(col) && is.numeric(col2)) {
        colour <- seq(col, col2, length.out = n)
    } else if (is.character(col) && is.character(col2)) {
        colour <- grDevices::colorRampPalette(c(col, col2))(n)
    } else if (is.null(col) && is.null(col2)){
        colour <- "black"
    }else {
        stop("col and col2 should be both numeric or character..." )
    }
    if (is.numeric(size1) && is.numeric(size2)){
        size <- seq(size1, size2, length.out=n)
    }else if (is.null(size1) && is.null(size2)){
        size <- 0.5
    }
    dat <- data.frame(x = x,
               xend = xend,
               y = y,
               yend = yend,
               colour = colour,
               size = size)
    return(dat)
}

setup_data_continuous_color_size_tree <- function(df, nsplit = 100, extend = 0.002, continuous = "colour") {
    lapply(1:nrow(df), function(i) {
        df2 <- setup_data_continuous_color_size(x = df$x[i],
                                                xend = df$xend[i],
                                                y = df$y[i],
                                                yend = df$yend[i],
                                                col = df$col[i],
                                                col2 = df$col2[i],
                                                size1 = df$size1[i],
                                                size2 = df$size2[i],
                                                xrange = range(df$x),
                                                nsplit = nsplit,
                                                extend = extend)
        df2$node <- df$node[i]
        # for aes(size=I(variable)) etc.
        if (continuous %in% c("color", "colour")){
            j <- match(c('x', 'xend', 'y', 'yend', 'col', 'col2', 'colour', 'size1', 'size2'), colnames(df))
            df2$size <- NULL
        }else if (continuous == "size"){
            j <- match(c("x", "xend", "y", "yend", "col", "col2", "size1", "size2", "size"), colnames(df))
            df2$colour <- NULL
        }else if (continuous == "all"){
            j <- match(c('x', 'xend', 'y', 'yend', 'col', 'col2', 'colour', 'size1', 'size2', 'size'), colnames(df))
        }
        j <- j[!is.na(j)]
        merge(df[i, -j, drop = FALSE], df2, by = "node")
    }) %>% do.call('rbind', .)
}
