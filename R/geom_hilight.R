#' layer of hilight clade
#' 
#' `geom_hilight` supports data.frame as input. And aesthetics of layer can be mapped.
#' you can see the Aesthetics section to set parameters. 
#'
#' @title geom_hilight 
#' @rdname geom-hilight
#' @param data data.frame, The data to be displayed in this layer, default is NULL.
#' @param mapping Set of aesthetic mappings, default is NULL.
#' @param node selected node to hilight, when data and mapping is NULL, it is required.
#' @param type the type of layer, default is `auto`, meaning rectangular, circular,
#' slanted, fan, inward_circular, radial, equal_angle, ape layout tree will use rectangular layer,
#' unrooted and daylight layout tree use will use encircle layer. You can specify this parameter to
#' `rect` (rectangular layer) or `encircle` (encircle layer).
#' @param ... additional parameters, see also Aesthetics section.
#' @section Aesthetics:
#' \code{geom_hilight()} understands the following aesthetics for rectangular layer (required 
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{node}} selected node to hight light, it is required.
#'        \item \code{colour} the colour of margin, default is NA.
#'        \item \code{fill} the colour of fill, default is 'steelblue'.
#'        \item \code{alpha} the transparency of fill, default is 0.5.
#'        \item \code{extend} extend xmax of the rectangle, default is 0.
#'        \item \code{extendto} specify a value, meaning the rectangle extend to, default is NULL.
#'        \item \code{linetype} the line type of margin, default is 1.
#'        \item \code{size} the width of line of margin, default is 0.5.
#'     }
#' \code{geom_hilight()} understands the following aesthethics for encircle layer (required 
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{node}} selected node to hight light, it is required.
#'        \item \code{colour} the colour of margin, default is 'black'.
#'        \item \code{fill} the colour of fill, default is 'steelblue'.
#'        \item \code{alpha} the transparency of fill, default is 0.5.
#'        \item \code{expand} expands the xspline clade region, default is 0.
#'        \item \code{spread} control the size, when only one point.
#'        \item \code{size} the width of line of margin, default is 0.5.
#'        \item \code{linetype} the line type of margin, default is 1.
#'        \item \code{s_shape} the shape of the spline relative to the control points, default is 0.5.
#'        \item \code{s_open}  whether the spline is a line or a closed shape, default is FALSE.
#'     }
#' @return a list object.
#' @author Guangchuang Yu and Shuangbin Xu
#' @export
#' @examples 
#' library(ggplot2)
#' set.seed(102)
#' tree <- rtree(60)
#' p <- ggtree(tree)
#' p1 <- p + geom_hilight(node=62) + geom_hilight(node=88, fill="red")
#' p1
#' dat <- data.frame(id=c(62, 88), type=c("A", "B"))
#' p2 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type))
#' p2
geom_hilight <- function(data=NULL,
                         mapping=NULL,
                         node=NULL,
                         type="auto",
                          ...){
    params <- list(...)
    structure(list(data    = data,
                   mapping = mapping,
                   node    = node,
                   type    = type,
                   params  = params),
              class = 'hilight')
}

##' @rdname geom-hilight
##' @export
geom_highlight <- geom_hilight

geom_hilight_rect2 <- function(data=NULL,
                               mapping=NULL,
                               stat = 'identity',
                               position = 'identity',
                               show.legend = NA,
                               linejoin = "mitre",
                               na.rm = FALSE,
                               inherit.aes = FALSE,
                               ...){
    layer(data = data, 
          mapping = mapping, 
          stat = stat, 
          geom = GeomHilightRect,
          position = position, 
          show.legend = show.legend, 
          inherit.aes = inherit.aes,
          params = list(linejoin = linejoin, 
                        na.rm = na.rm, 
                        ...))
}

#' @importFrom ggplot2 draw_key_polygon Geom ggproto aes GeomPolygon
#' @importFrom grid rectGrob gpar grobTree
GeomHilightRect <- ggproto("GeomHilightRect", Geom,
                           default_aes = aes(colour = NA, fill = "steelblue", 
                                             size = 0.5, linetype = 1, alpha = 0.5,
                                             extend=0, extendto=NULL),
                           required_aes = c("xmin", "xmax", "ymin", "ymax", "clade_root_node"),
                           draw_key = draw_key_polygon,
                           draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                               data$xmax <- data$xmax + data$extend
                               if (!any(is.null(data$extendto)) && !any(is.na(data$extendto))){
                                   # check whether the x of tree is reversed.
                                   flag1 <- data$xmin < data$xmax
                                   # check whether extendto is more than xmax 
                                   flag2 <- data$extendto < data$xmax
                                   flag <- equals(flag1, flag2)
                                   if (all(flag1) && any(flag)){
                                       warning_wrap("extendto ", 
                                                    paste0(data$extendto[flag], collapse="; "), 
                                                    ifelse(length(data$extendto[flag])>1, " are", " is"), 
                                                    " too small for node: ", paste0(data$clade_root_node[flag], collapse="; "),
                                                    ", keep the original xmax value(s): ", paste0(data$xmax[flag], collapse="; "), ".")
                                       data$xmax[!flag] <- data$extendto[!flag]
                                   }else if(!all(flag1) && any(flag)){
                                       warning_wrap("extendto ", 
                                                    paste0(data$extendto[flag], collapse="; "), 
                                                    ifelse(length(data$extendto[flag])>1, " are", " is"),
                                                    " too big for node: ", paste0(data$clade_root_node[flag], collapse="; "),
                                                    ", keep the original xmax value(s): ", paste0(data$xmax[flag], collapse="; "), ".")
                                       data$xmax[!flag] <- data$extendto[!flag]
                                   }else{
                                       data$xmax <- data$extendto 
                                   }
                               }
                               if (!coord$is_linear()) {
                                   aesthetics <- setdiff(names(data), c("xmin", "xmax", "ymin", "ymax", "clade_root_node"))
                                   polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                                                 poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                                                 aes <- new_data_frame(row[aesthetics])[rep(1,5), ]
                                                 GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
                                                 })
                                   ggname("bar", do.call("grobTree", polys))
                               }else{
                                   coords <- coord$transform(data, panel_params)
                                   ggname("geom_hilight_rect2", rectGrob(
                                           coords$xmin, coords$ymax,
                                           width = coords$xmax - coords$xmin,
                                           height = coords$ymax - coords$ymin,
                                           default.units = "native",
                                           just = c("left", "top"),
                                           gp = gpar(col = coords$colour,
                                                     fill = alpha(coords$fill, coords$alpha),
                                                     lwd = coords$size * ggplot2:::.pt,
                                                     lty = coords$linetype,
                                                     linejoin = linejoin,
                                                     lineend = if (identical(linejoin, "round")) "round" else "square")
                                         ))
                               }
                           }

                          )


geom_hilight_encircle2 <- function(data=NULL,
                                   mapping=NULL,
                                   stat = 'identity',
                                   position = 'identity',
                                   show.legend = NA,
                                   inherit.aes=FALSE,
                                   na.rm=FALSE,...){
    layer(data=data,
          mapping=mapping,
          stat=stat,
          geom=GeomHilightEncircle,
          position=position,
          show.legend=show.legend,
          inherit.aes=inherit.aes,
          params=list(na.rm=na.rm,
                      ...)
          )
}

GeomHilightEncircle <- ggproto("GeomHilightEncircle", Geom,
                                required_aes = c("x", "y", "clade_root_node"),
                                default_aes = aes(colour="black", fill="steelblue", alpha = 0.5,
                                                  expand=0, spread=0.1, linetype=1, size=0.5,
                                                  s_shape=0.5, s_open=FALSE),
                                draw_key = draw_key_polygon,
                                draw_panel = function(data, panel_scales, coord){
                                    globs <- lapply(split(data, data$clade_root_node), function(i)
                                                   get_glob_encircle(i, panel_scales, coord))
                                    ggname("geom_hilight_encircle2", do.call("grobTree", globs))
                                }
                                
                        )

##' get position of clade (xmin, xmax, ymin, ymax)
##'
##'
##' @title get_clade_position
##' @param treeview tree view
##' @param node selected node
##' @return data.frame
##' @export
##' @author Guangchuang Yu
get_clade_position <- function(treeview, node) {
  get_clade_position_(treeview$data, node)
}

get_clade_position_ <- function(data, node, reverse=FALSE) {
    sp <- tryCatch(offspring.tbl_tree(data, node)$node, error=function(e) NULL)
    i <- match(node, data$node)
    if (is.null(sp)) {
        ## tip
        sp.df <- data[i,]
    } else {
        sp <- c(sp, node)
        sp.df <- data[match(sp, data$node),]
    }

    x <- sp.df$x
    y <- sp.df$y

    if ("branch.length" %in% colnames(data)) {
        if (reverse){
            xmin <- min(x, na.rm=TRUE)
            xmax <- max(x, na.rm=TRUE) + data[["branch.length"]][i]/2
        }else{
            xmin <- min(x, na.rm=TRUE) - data[["branch.length"]][i]/2
            xmax <- max(x, na.rm=TRUE)
        }
    } else {
        xmin <- min(sp.df$branch, na.rm=TRUE)
        xmax <- max(x, na.rm=TRUE)
    }
    data.frame(xmin=xmin,
               xmax=xmax,
               ymin=min(y, na.rm=TRUE) - 0.5,
               ymax=max(y, na.rm=TRUE) + 0.5)
}

#' @importFrom utils getFromNamespace
warning_wrap <- getFromNamespace("warning_wrap", "ggplot2")
rect_to_poly <- getFromNamespace("rect_to_poly", "ggplot2")
new_data_frame <- getFromNamespace("new_data_frame", "ggplot2")

## ##' layer of hilight clade with rectangle
## ##'
## ##' @title geom_hilight
## ##' @rdname geom-hilight
## ##' @param node selected node to hilight (required)
## ##' @param fill color fill (default = steelblue)
## ##' @param alpha alpha transparency, (default = 0.5)
## ##' @param extend extend xmax of the rectangle (default = 0)
## ## @param extendto extend xmax to extendto (default = NULL), only works for rectangular and circular/fan layouts
## ##' @param ... additional parameters
## ##' @return ggplot2
## ##' @export
## ##' @importFrom ggplot2 aes_
## ##' @importFrom ggplot2 GeomRect
## ##' @author Guangchuang Yu
## geom_hilight <- function(node, fill="steelblue", alpha=.5, extend=0, ...) {
##     structure(list(node = node,
##                    fill = fill,
##                    alpha = alpha,
##                    extend = extend,
##                    ## extendto = extendto,
##                    ...),
##               class = 'hilight')
## }

## ##' @rdname geom-hilight
## ##' @export
## geom_highlight <- geom_hilight

## geom_hilight_rectangular <- function(node, mapping = NULL, fill="steelblue",
##                                      alpha=.5, extend=0, extendto=NULL, ...) {
##   data = NULL
##   stat = "hilight"
##   position = "identity"
##   show.legend = NA
##   na.rm = TRUE
##   inherit.aes = FALSE
##   check.aes = FALSE
## 
##   default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch = ~branch)
## 
##   if (is.null(mapping)) {
##       mapping <- default_aes
##   } else {
##       mapping <- modifyList(default_aes, mapping)
##   }
## 
## 
##   layer(
##     stat=StatHilight,
##     data = data,
##     mapping = mapping,
##     geom = GeomRect,
##     position = position,
##     show.legend=show.legend,
##     inherit.aes = inherit.aes,
##     check.aes = check.aes,
##     params = list(node=node,
##                   fill=fill,
##                   alpha=alpha,
##                   extend=extend,
##                   extendto=extendto,
##                   na.rm = na.rm,
##                   ...)
##   )
## }

## ##' stat_hilight
## ##'
## ##' @title stat_hilight
## ##' @param mapping aes mapping
## ##' @param data data
## ##' @param geom geometric object
## ##' @param position position
## ##' @param node node number
## ##' @param show.legend show legend
## ##' @param inherit.aes logical
## ##' @param fill fill color
## ##' @param alpha transparency
## ##' @param extend extend xmax of the rectangle
## ##' @param extendto extend xmax to extendto
## ##' @param ... additional parameter
## ##' @return layer
## ##' @importFrom ggplot2 layer
## ##' @export
## stat_hilight <- function(mapping=NULL, data=NULL, geom="rect",
##                          position="identity",  node,
##                          show.legend=NA, inherit.aes=FALSE,
##                          fill, alpha, extend=0, extendto=NULL,
##                          ...) {
## 
##   default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch=~branch) #, branch.length=~branch.length)
## 
##   if (is.null(mapping)) {
##     mapping <- default_aes
##   } else {
##     mapping <- modifyList(default_aes, mapping)
##   }
## 
##   layer(
##     stat=StatHilight,
##     data = data,
##     mapping = mapping,
##     geom = geom,
##     position = position,
##     show.legend=show.legend,
##     inherit.aes = inherit.aes,
##     params = list(node=node,
##                   fill=fill,
##                   alpha=alpha,
##                   extend=extend,
##                   extendto=extendto,
##                   ...)
##   )
## }
## 
## ##' StatHilight
## ##' @rdname ggtree-ggproto
## ##' @format NULL
## ##' @usage NULL
## ##' @importFrom ggplot2 Stat
## ##' @export
## StatHilight <- ggproto("StatHilight", Stat,
##                        compute_group = function(self, data, scales, params, node, extend, extendto) {
##                            df <- get_clade_position_(data, node)
##                            df$xmax <- df$xmax + extend
##                            if (!is.null(extendto) && !is.na(extendto)) {
##                                if (extendto < df$xmax) {
##                                    warning_wrap("extendto is too small for node: ", node, 
##                                                 ", keep the original xmax value: ", df$xmax, ".")
##                                } else {
##                                    df$xmax <- extendto
##                                }
##                            }
##                            return(df)
##                        },
##                        required_aes = c("x", "y") #, "branch.length")
##                        )

