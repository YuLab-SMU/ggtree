#' layer of hilight clade
#' 
#' `geom_hilight2` supports data.frame as input. And aesthetics of layer can be mapped.
#' you can see the Aesthetics section to set parameters. 
#' 
#'
#' @param data data.frame, The data to be displayed in this layer, default is NULL.
#' @param mapping Set of aesthetic mappings, default is NULL.
#' @param node selected node to hilight, when data and mapping is NULL, it is required.
#' @param ... additional parameters, see also Aesthetics section.
#' @section Aesthetics:
#' \code{geom_hilight2()} understands the following aesthetics for rectangular, circular,
#' slanted, fan, inward_circular, radial, equal_angle, ape layout tree (required aesthetics 
#' are in bold):
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
#' \code{geom_hilight2()} understands the following aesthethics for 
#'     unrooted and daylight layout tree (required aesthetics are in bold):
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
#' p1 <- p + geom_hilight2(node=62) + geom_hilight2(node=88, fill="red")
#' p1
#' dat <- data.frame(id=c(62, 88), type=c("A", "B"))
#' p2 <- p + geom_hilight2(data=dat, mapping=aes(node=id, fill=type))
#' p2
geom_hilight2 <- function(data=NULL,
                          mapping=NULL,
                          node=NULL,
                          ...){
    params <- list(...)
    structure(list(data    = data,
                   mapping = mapping,
                   node    = node,
                   params  = params),
              class = 'hilight2')
}


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
#' @importFrom grid rectGrob gpar
GeomHilightRect <- ggproto("GeomHilightRect", Geom,
                           default_aes = aes(colour = NA, fill = "steelblue", 
                                             size = 0.5, linetype = 1, alpha = 0.5,
                                             extend=0, extendto=NULL),
                           required_aes = c("xmin", "xmax", "ymin", "ymax", "clade_root_node"),
                           draw_key = draw_key_polygon,
                           draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                               data$xmax <- data$xmax + data$extend
                               if (!is.null(data$extendto) && !is.na(data$extendto)){
                                   flag <- data$extendto < data$xmax
                                   if (any(flag)){
                                       warning_wrap("extendto is too small for node: ", paste0(data$clade_root_node[flag], collapse="; "),
                                                    ", keep the original xmax value: ", paste0(data$xmax[flag], collapse="; "), ".")
                                       data$xmax[!flag] <- data$xmax[!flag] + data$extendto[!flag]
                                   }else{
                                       data$xmax <- data$xmax + data$extendto 
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

rect_to_poly <- getFromNamespace("rect_to_poly", "ggplot2")
new_data_frame <- getFromNamespace("new_data_frame", "ggplot2")
