#' layer of hilight clade
#' 
#' `geom_hilight` supports data.frame as input. And aesthetics of layer can be mapped.
#' you can see the Aesthetics section to set parameters. 
#'
#' @title geom_hilight 
#' @rdname geom-hilight
#' @param data data.frame, The data to be displayed in this layer, defaults to NULL.
#' @param mapping Set of aesthetic mappings, defaults to NULL.
#' @param node selected node to hilight, when data and mapping is NULL, it is required.
#' @param type the type of layer, defaults to `auto`, meaning rectangular, circular,
#' slanted, fan, inward_circular, radial, equal_angle, ape layout tree will use rectangular layer,
#' unrooted and daylight layout tree use will use encircle layer. You can specify this parameter to
#' `rect` (rectangular layer) or `encircle` (encircle layer), 'gradient' (gradient color), 
#' 'roundrect' (round rectangular layer).
#' @param to.bottom logical, whether set the high light layer to the bottom in all layers of 'ggtree'
#' object, default is FALSE.
#' @param ... additional parameters, see also the below and Aesthetics section.
#'     \itemize{
#'        \item \code{align} control the align direction of the edge of high light rectangular.
#'          Options is 'none' (default), 'left', 'right', 'both'. This argument only work when the
#'          'geom_hilight' is plotting using geom_hilight(mapping=aes(...)).
#'        \item \code{gradient.direction} character, the direction of gradient color, defaults to 'rt'
#'          meaning the locations of gradient color is from root to tip, options are 'rt' and 'tr'.
#'        \item \code{gradient.length.out} integer, desired length of the sequence of gradient color,
#'          defaults to 2.
#'        \item \code{roundrect.r} numeric, the radius of the rounded corners, when \code{roundrect=TRUE},
#'          defaults to 0.05.
#'     }
#' @section Aesthetics:
#' \code{geom_hilight()} understands the following aesthetics for rectangular layer (required 
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{node}} selected node to hight light, it is required.
#'        \item \code{colour} the colour of margin, defaults to NA.
#'        \item \code{fill} the colour of fill, defaults to 'steelblue'.
#'        \item \code{alpha} the transparency of fill, defaults to 0.5.
#'        \item \code{extend} extend xmax of the rectangle, defaults to 0.
#'        \item \code{extendto} specify a value, meaning the rectangle extend to, defaults to NULL.
#'        \item \code{linetype} the line type of margin, defaults to 1.
#'        \item \code{linewidth} the width of line of margin, defaults to 0.5.
#'     }
#' \code{geom_hilight()} understands the following aesthethics for encircle layer (required 
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{node}} selected node to hight light, it is required.
#'        \item \code{colour} the colour of margin, defaults to 'black'.
#'        \item \code{fill} the colour of fill, defaults to 'steelblue'.
#'        \item \code{alpha} the transparency of fill, defaults to 0.5.
#'        \item \code{expand} expands the xspline clade region, defaults to 0.
#'        \item \code{spread} control the size, when only one point.
#'        \item \code{linewidth} the width of line of margin, defaults to 0.5.
#'        \item \code{linetype} the line type of margin, defaults to 1.
#'        \item \code{s_shape} the shape of the spline relative to the control points, defaults to 0.5.
#'        \item \code{s_open}  whether the spline is a line or a closed shape, defaults to FALSE.
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
#' p3 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type), align="left")
#' p4 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type), align="right")
#' p5 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type), align="both")
#' # display the high light layer with gradiental color rectangular.
#' p6 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type), type = "gradient", alpha=0.68)
#' p7 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type), 
#'                       type = "gradient", gradient.direction="tr", alpha=0.68)
#' # display the high light layer with round rectangular.
#' p8 <- p + geom_hilight(data=dat, mapping=aes(node=id, fill=type), type = "roundrect", alpha=0.68)
#' p2/ p3/ p4/ p5 / p6/ p7/ p8
#' 
#' library(ggiraph)
#' f <- ggtree(tree, layout = 'daylight', 
#'             mapping = aes(tooltip = round(branch.length, 2), data_id = node))
#' f1 <- f + geom_hilight(data=dat, 
#'             mapping=aes(node=id, fill=type, tooltip = id, data_id = id), to.bottom = TRUE)
#' ff <- ggtree(tree, layout = 'circular', 
#'              mapping = aes(tooltip = round(branch.length, 2), data_id = node))
#' ff1 <- ff + geom_hilight(
#'               data=dat, 
#'               mapping = aes(node=id, fill=type, tooltip = id, data_id = id), 
#'               to.bottom = TRUE
#'             )
#' girafe(ggobj = f1)
#' girafe(ggobj = ff1)
#' @references  
#' For more detailed demonstration, please refer to chapter 5.2.2 of 
#' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
#' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_hilight <- function(data=NULL,
                         mapping=NULL,
                         node=NULL,
                         type="auto",
                         to.bottom=FALSE,
                          ...){
    params <- list(...)
    structure(list(data    = data,
                   mapping = mapping,
                   node    = node,
                   type    = type,
                   to.bottom = to.bottom,
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
#' @importFrom cli cli_alert_warning
GeomHilightRect <- ggproto("GeomHilightRect", Geom,
                           default_aes = aes(colour = NA, fill = "steelblue", 
                                             linewidth = 0.5, linetype = 1, alpha = 0.5,
                                             extend=0, extendto=NULL),
                           required_aes = c("xmin", "xmax", "ymin", "ymax", "clade_root_node"),
                           draw_key = draw_key_polygon,
                           rename_size = TRUE,
                           make_hilight_data = function(data, align = "none"){
                               data$xmax <- data$xmax + data$extend
                               if (!any(is.null(data$extendto)) && !any(is.na(data$extendto))){
                                   # check whether the x of tree is reversed.
                                   flag1 <- data$xmin < data$xmax
                                   # check whether extendto is more than xmax
                                   flag2 <- data$extendto < data$xmax
                                   flag <- flag1 == flag2
                                   if (all(flag1) && any(flag)){
                                       cli_alert_warning(c("{.code extendto} ", paste0(data$extendto[flag], collapse="; "),
                                                    ifelse(length(data$extendto[flag])>1, " are", " is")," too small for node: ",
                                                    paste0(data$clade_root_node[flag], collapse="; "),", keep the original xmax value(s): ",
                                                    paste0(data$xmax[flag], collapse="; "), "."), wrap = TRUE)
                                       data$xmax[!flag] <- data$extendto[!flag]
                                   }else if(!all(flag1) && any(flag)){
                                       cli_alert_warning(c("{.code extendto} ", paste0(data$extendto[flag], collapse="; "),
                                                    ifelse(length(data$extendto[flag])>1, " are", " is"), " too big for node: ",
                                                    paste0(data$clade_root_node[flag], collapse="; "), ", keep the original xmax value(s): ",
                                                    paste0(data$xmax[flag], collapse="; "), "."), wrap = TRUE)
                                       data$xmax[!flag] <- data$extendto[!flag]
                                   }else{
                                       data$xmax <- data$extendto
                                   }
                               }
                               data <- build_align_data(data=data, align=align)
                               return(data)
                           
                           },
                           draw_panel = function(self, data, panel_params, coord, 
                                                 linejoin = "mitre", align="none", 
                                                 gradient = FALSE, 
                                                 gradient.direction = "rt",
                                                 gradient.length.out = 2,
                                                 roundrect = FALSE,
                                                 roundrect.r = 0.05
                                                 ){
                               data <- GeomHilightRect$make_hilight_data(data, align)
                               if (!coord$is_linear()) {
                                   if (gradient){
                                       cli_alert_warning("The gradient color hight light layer only presents in 
                                                         rectangular, ellipse, roundrect layouts.", wrap = TRUE)
                                   }
                                   if (roundrect){
                                       cli_alert_warning("The round rectangular hight light layer only presents in 
                                                         rectangular, ellipse, roundrect layouts.", wrap =TRUE)
                                   }
                                   aesthetics <- setdiff(colnames(data), #"x.start", "y.start", "x.stop", "y.stop"), 
                                                         c("xmin", "xmax", "ymin", "ymax", "clade_root_node"))
                                   
                                   polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                                                 poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                                                 aes <- row[rep(1,5), aesthetics] 
                                                 GeomPolygon$draw_panel(vctrs::vec_cbind(poly, aes), panel_params, coord)
                                                 })
                                   ggname("geom_hilight_rect2", do.call("grobTree", polys))
                               }else{
                                   coords <- coord$transform(data, panel_params)
                                   hilightGrob <- ifelse(roundrect, grid::roundrectGrob, grid::rectGrob)
                                   if (gradient){
                                       if (roundrect){
                                           cli_alert_warning("The round rectangular and gradient are not applied simultaneously")
                                       }
                                       gradient.direction <- match.arg(gradient.direction, c("rt", "tr"))
                                       rects <- lapply(split(coords, seq_len(nrow(coords))), function(row){
                                                     fill <- grid::linearGradient(
                                                                 x1 = 0,
                                                                 x2 = 1,
                                                                 y1 = 0.5,
                                                                 y2 = 0.5,
                                                                 colours = if(gradient.direction == "rt"){
                                                                               alpha(c(row$fill, "white"), row$alpha)
                                                                           }else{
                                                                               rev(alpha(c(row$fill, "white"), row$alpha))
                                                                           },
                                                                 stops = seq(0, 1, length.out = gradient.length.out)
                                                             )
                                                     rectGrob(
                                                         x = row$xmin, y = row$ymax, 
                                                         width = row$xmax - row$xmin, 
                                                         height = row$ymax - row$ymin,
                                                         default.units = "native",
                                                         just = c("left", "top"),
                                                         gp = gpar(col = row$colour,
                                                                   fill = fill,
                                                                   lwd = row$linewidth * ggplot2:::.pt,
                                                                   lty = row$linetype,
                                                                   linejoin = linejoin,
                                                                   lineend = if (identical(linejoin, "round")) "round" else "square")
                                                     )
                                                 })
                                       ggname("geom_hilight_rect2", do.call("grobTree", rects))
                                   }else{
                                       if (roundrect){
                                           rects <- lapply(split(coords, seq_len(nrow(coords))), function(row) 
                                                        grid::roundrectGrob(
                                                            row$xmin, row$ymax,
                                                            width = row$xmax - row$xmin,
                                                            height = row$ymax - row$ymin,
                                                            r = grid::unit(roundrect.r, "snpc"),
                                                            default.units = "native",
                                                            just = c("left", "top"),
                                                            gp = grid::gpar(
                                                              col = row$colour,
                                                              fill = alpha(row$fill, row$alpha),
                                                              lwd = row$linewidth * ggplot2::.pt,
                                                              lty = row$linetype,
                                                              lineend = "butt"
                                                            )
                                                        )
                                                    )
                                           ggname("geom_hilight_rect2", do.call("grobTree", rects)) 
                                       }else{
                                           ggname("geom_hilight_rect2", rectGrob(
                                                  coords$xmin, coords$ymax,
                                                  width = coords$xmax - coords$xmin,
                                                  height = coords$ymax - coords$ymin,
                                                  default.units = "native",
                                                  just = c("left", "top"),
                                                  gp = gpar(col = coords$colour,
                                                            fill = alpha(coords$fill, coords$alpha),
                                                            lwd = coords$linewidth * ggplot2:::.pt,
                                                            lty = coords$linetype,
                                                            linejoin = linejoin,
                                                            lineend = if (identical(linejoin, "round")) "round" else "square")
                                             ))
                                       }
                                   }
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

check_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    warning(paste0(
      "Using the `size` aesthetic with ", name, " was deprecated in ggplot2 3.4.0.\n",
      "Please use the `linewidth` aesthetic instead."
    ))
    data$linewidth <- data$size
  }
  data
}
snake_class <- getFromNamespace('snake_class', 'ggplot2')
snakeize <- getFromNamespace('snakeize', 'ggplot2')

#' @title ggproto classes of ggtree
#' @description
#' ggproto classes of ggtree
#' @format NULL
#' @usage NULL
#' @export
GeomHilightEncircle <- ggproto("GeomHilightEncircle", Geom,
                                required_aes = c("x", "y", "clade_root_node"),
                                default_aes = aes(colour="black", fill="steelblue", alpha = 0.5,
                                                  expand=0, spread=0.1, linetype=1, linewidth = 0.5,
                                                  s_shape=0.5, s_open=FALSE),
                                draw_key = draw_key_polygon,
                                rename_size = TRUE,
                                draw_panel = function(self, data, panel_scales, coord){
                                    data <- check_linewidth(data, snake_class(self))
                                    globs <- lapply(split(data, data$clade_root_node), function(i)
                                                   get_glob_encircle(i, panel_scales, coord))
                                    ggname("geom_hilight_encircle2", do.call("grobTree", globs))
                                }
                                
                        )


#draw_panel_polar <- function (data, panel_params, coord, rule = "evenodd", gradient, gradient.direction, gradient.length.out){
#    n <- nrow(data)
#    if (n == 1)
#        return(zeroGrob())
#    munched <- ggplot2::coord_munch(coord, data, panel_params)
#    if (is.null(munched$subgroup)) {
#        munched <- munched[order(munched$group), ]
#        first_idx <- !duplicated(munched$group)
#        first_rows <- munched[first_idx, ]
#        if (gradient){
#            gradient.direction <- match.arg(gradient.direction, c("rt", "tr"))
#            rects <- lapply(split(munched, munched$group), function(row){
#                            fill <- grid::linearGradient(
#                                        x1 = unique(row$x.start), 
#                                        x2 = unique(row$x.stop),
#                                        y1 = 0,
#                                        y2 = 1,
#                                        default.units = "native",
#                                        colours = if (gradient.direction=="rt"){
#                                                     alpha(c(first_rows$fill, "white"), first_rows$alpha) 
#                                                  }else{
#                                                     rev(alpha(c(first_rows$fill, "white"), first_rows$alpha))
#                                                  },
#                                        stops = seq(0, 1, length.out = gradient.length.out)
#                                    )
#                            grid::polygonGrob(
#                                row$x, row$y, id = row$group,
#                                default.units = "native",
#                                gp = gpar(col = first_rows$colour,
#                                          fill = fill,
#                                          lwd = first_rows$size * ggplot2:::.pt,
#                                          lty = first_rows$linetype)
#                            )
#                     })
#            ggname("geom_polygon2", do.call("grobTree", rects))   
#        }else{
#            ggname("geom_polygon2", grid::polygonGrob(munched$x, munched$y,
#               default.units = "native", id = munched$group, gp = gpar(col = first_rows$colour,
#               fill = alpha(first_rows$fill, first_rows$alpha),
#               lwd = first_rows$size * ggplot2::.pt, lty = first_rows$linetype)))
#        }
#    }
#    else {
#        if (utils::packageVersion("grid") < "3.6") {
#            abort("Polygons with holes requires R 3.6 or above")
#        }
#        munched <- munched[order(munched$group, munched$subgroup), ]
#        id <- match(munched$subgroup, unique(munched$subgroup))
#        first_idx <- !duplicated(munched$group)
#        first_rows <- munched[first_idx, ]
#        if (gradient){
#            gradient.direction <- match.arg(gradient.direction, c("rt", "tr"))
#            rects <- lapply(split(munched, munched$group), function(row){
#                            fill <- grid::linearGradient(
#                                        x1 = first_rows$x.start,
#                                        x2 = first_rows$x.stop,
#                                        y1 = first_rows$y.start,
#                                        y2 = first_rows$y.stop,
#                                        default.units = "native",
#                                        colours = if (gradient.direction=="rt"){
#                                                     alpha(c(first_rows$fill, "white"), first_rows$alpha)
#                                                  }else{
#                                                     rev(alpha(c(first_rows$fill, "white"), first_rows$alpha))
#                                                  },
#                                        stops = seq(0, 1, length.out = gradient.length.out)
#                                    )
#                            grid::pathGrob(
#                                row$x, row$y, id = match(row$subgroup, unique(row$group)), 
#                                pathId = munched$group,
#                                rule = rule, default.units = "native",
#                                gp = gpar(col = first_rows$colour,
#                                          fill = fill,
#                                          lwd = first_rows$size * ggplot2:::.pt,
#                                          lty = first_rows$linetype)
#                            )
#                     })
#            ggname("geom_polygon2", do.call("grobTree", rects)) 
#        }else{
#            ggname("geom_polygon2", grid::pathGrob(munched$x, munched$y,
#                default.units = "native", id = id, pathId = munched$group,
#                rule = rule, gp = gpar(col = first_rows$colour, fill = alpha(first_rows$fill,
#                first_rows$alpha), lwd = first_rows$size * ggplot2::.pt,
#                lty = first_rows$linetype)))
#        }
#    }
#}

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
    sp <- tryCatch(.offspring.tbl_tree(data, node)$node, error=function(e) NULL)
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

    w <- getOption("clade_width_extend", default = 0.5)
    data.frame(xmin=xmin,
               xmax=xmax,
               ymin=min(y, na.rm=TRUE) - w,
               ymax=max(y, na.rm=TRUE) + w)
}

build_align_data <- function(data, align){
    align <- match.arg(align, c("none", "left", "right", "both"))
    data <- switch(align,
              none = {
                  data
              },
              left = {
                  data$xmin <- min(data$xmin, na.rm = TRUE)
                  data
              },
              right = {
                  data$xmax <- max(data$xmax, na.rm = TRUE)
                  data
              },
              both ={
                  data$xmin <- min(data$xmin, na.rm = TRUE)
                  data$xmax <- max(data$xmax, na.rm = TRUE)
                  data
              }
            )
    return (data)
}





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

#' @importFrom utils getFromNamespace
data_frame0 <- getFromNamespace("data_frame0", "ggplot2")


# taken from ggplot2 as it was removed in recent release.
# 
# Convert rectangle to polygon
# Useful for non-Cartesian coordinate systems where it's easy to work purely in
# terms of locations, rather than locations and dimensions. Note that, though
# `polygonGrob()` expects an open form, closed form is needed for correct
# munching (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-458406857).
#
# @keyword internal
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data_frame0(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}

# the internal functions of ggiraph
layer_interactive <- getFromNamespace("layer_interactive", "ggiraph")
add_default_interactive_aes <- getFromNamespace("add_default_interactive_aes", "ggiraph")
interactive_geom_parameters <- getFromNamespace("interactive_geom_parameters", "ggiraph")
interactive_geom_draw_key <- getFromNamespace("interactive_geom_draw_key", "ggiraph")
IPAR_NAMES <- getFromNamespace("IPAR_NAMES", "ggiraph")
add_interactive_attrs <- getFromNamespace("add_interactive_attrs", "ggiraph")


#' @title ggproto classes for ggiraph
#' @description
#' ggproto classes for ggiraph
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom grid gTree gpar curveGrob grobTree
#' @importFrom cli cli_alert_warning
#' @importFrom ggiraph GeomInteractivePolygon
#' @export
GeomInteractiveHilightRect <- ggproto(
  "GeomInteractiveHilightRect",
  GeomHilightRect,
  default_aes = add_default_interactive_aes(GeomHilightRect),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  draw_panel = function(data, panel_params, coord, align='none',
                        gradient = FALSE, roundrect = FALSE, ...,
                        .ipar = IPAR_NAMES){
     if (!.check_ipar_params(data)){
        return(GeomHilightRect$draw_panel(data = data,
                                          panel_params = panel_params,
                                          coord = coord,
                                          align = align,
                                          gradient = gradient,
                                          roundrect = roundrect,
                                          ...
                        )

        )
     }
     if (coord$is_linear()){
        gr <- GeomHilightRect$draw_panel(data, panel_params, coord, gradient = gradient, roundrect = roundrect, align = align, ...)
        coords <- coord$transform(data, panel_params)
        gr <- add_interactive_attrs(gr, coords, ipar=.ipar)
     }else{
        data <- GeomHilightRect$make_hilight_data(data, align)
        if (gradient){
            cli_alert_warning("The gradient color hight light layer only presents in
                              rectangular, ellipse, roundrect layouts.", wrap = TRUE)
        }
        if (roundrect){
            cli_alert_warning("The round rectangular hight light layer only presents in
                              rectangular, ellipse, roundrect layouts.", wrap =TRUE)
        }
        aesthetics <- setdiff(colnames(data), c("xmin", "xmax", "ymin", "ymax", "clade_root_node"))
        gr <- lapply(split(data, seq_len(nrow(data))), function(row) {
                      poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                      aes <- row[rep(1,5), aesthetics]
                      GeomInteractivePolygon$draw_panel(vctrs::vec_cbind(poly, aes), panel_params, coord, ..., .ipar=.ipar)
                      })
        gr <- ggname("geom_hilight_rect2_interactive", do.call("grobTree", gr))
     }
     gr
  }
)

#' @title ggproto classes for ggiraph
#' @description
#' ggproto classes for ggiraph
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveHilightEncircle <- ggproto(
  "GeomInteractiveHilightEncircle",
  GeomHilightEncircle,
  default_aes = add_default_interactive_aes(GeomHilightEncircle),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  draw_panel = function(self, data, panel_scales, coord, .ipar = IPAR_NAMES){
      if (!.check_ipar_params(data)){
         return(GeomHilightEncircle$draw_panel(data = data,
                                               panel_scales = panel_scales,
                                               coord = coord
                                               )
         )
      }
      data <- check_linewidth(data, snake_class(self))
      globs <- lapply(split(data, data$clade_root_node), function(i){
                      encircle <- get_glob_encircle(i, panel_scales, coord)
                      index <- duplicated(i[,!names(i) %in% c("x", "y")])
                      add_interactive_attrs(encircle, i[!index,,drop=FALSE], ipar=.ipar)
         }
      )      
      ggname("geom_hilight_encircle2_interactive", do.call("grobTree", globs)) 
  }
)

