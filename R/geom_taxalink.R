##' link between taxa
##'
##'
##' @title geom_taxalink
##' @param taxa1 taxa1, can be label or node number
##' @param taxa2 taxa2, can be label or node number
##' @param curvature A numeric value giving the amount of curvature.
##' Negative values produce left-hand curves,
##' positive values produce right-hand curves, and zero produces a straight line.
##' @param arrow specification for arrow heads, as created by arrow().
##' @param arrow.fill fill color to usse for the arrow head (if closed). `NULL` means use `colour` aesthetic.
##' @param offset numeric, control the shift of curve line (the ratio of axis value, 
##' range is "(0-1)"), default is NULL.
##' @param hratio numeric, the height of curve line, default is 1.
##' @param outward logical, control the orientation of curve when the layout of tree is circular, 
##' fan or other layout in polar coordinate, default is TRUE.
##' @param ... additional parameter.
##' @return ggplot layer
##' @export
##' @author Guangchuang Yu
geom_taxalink <- function(taxa1, taxa2, curvature=0.5, arrow = NULL, 
                          arrow.fill = NULL, offset=NULL, hratio=1, 
                          outward = TRUE, ...) {
    position = "identity"
    show.legend = NA
    na.rm = TRUE
    inherit.aes = FALSE

    mapping <- aes_(x=~x, y=~y, node=~node, label=~label, xend=~x, yend=~y)

    layer(stat=StatTaxalink,
          mapping=mapping,
          data = NULL,
          geom=GeomCurvelink,
          position='identity',
          show.legend=show.legend,
          inherit.aes = inherit.aes,
          params = list(taxa1 = taxa1,
                        taxa2 = taxa2,
                        curvature = curvature,
                        na.rm = na.rm,
                        arrow = arrow,
                        arrow.fill = arrow.fill,
                        offset = offset,
                        hratio = hratio,
                        outward = outward,
                        ...),
          check.aes = FALSE
          )
}

StatTaxalink <- ggproto("StatTaxalink", Stat,
                        compute_group = function(self, data, scales, params, taxa1, taxa2, offset) {
                            node1 <- taxa2node(data, taxa1)
                            node2 <- taxa2node(data, taxa2)
                            x <- data$x
                            y <- data$y
                            if (!is.null(offset)){
                                tmpshift <- offset * (max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
                                data.frame(x    = x[node1] + tmpshift,
                                           xend = x[node2] + tmpshift,
                                           y    = y[node1],
                                           yend = y[node2])
                            }else{

                                data.frame(x = x[node1],
                                           xend = x[node2],
                                           y = y[node1],
                                           yend = y[node2])
                            }
                        },
                        required_aes = c("x", "y", "xend", "yend")
                        )


geom_curvelink <- function(data=NULL, 
                           mapping=NULL, 
                           stat = "identity", 
                           position = "identity",
                           angle = 90,
                           arrow = NULL,
                           arrow.fill = NULL,
                           lineend = "butt",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,...){

    layer(
       data = data,
       mapping = mapping,
       stat = stat,
       geom = GeomCurvelink,
       position = position,
       show.legend = show.legend,
       inherit.aes = inherit.aes,
       params = list(
         arrow = arrow,
         arrow.fill = arrow.fill,
         angle = angle,
         lineend = lineend,
         na.rm = na.rm,
         ...
       )
    )

}

#' @importFrom ggplot2 GeomSegment
#' @importFrom grid gTree curveGrob gpar
#' @importFrom scales alpha
GeomCurvelink <- ggproto("GeomCurvelink", GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, curvature=0.5, hratio=1, ncp=1),
  draw_panel = function(data, panel_params, coord, angle = 90, shape=0.5, outward=TRUE,
                        arrow = NULL, arrow.fill=NULL, lineend = "butt", na.rm = FALSE) {
    if (!coord$is_linear()) {
        tmpgroup <- data$group
        starts <- subset(data, select = c(-xend, -yend))
        starts$group <- 1
        ends <- rename(subset(data, select = c(-x, -y)), c("x" = "xend", "y" = "yend"))
        ends$group <- 2
        pieces <- rbind(starts, ends)

        trans <- coord$transform(pieces, panel_params)
        starts <- trans[trans$group==1, ,drop=FALSE]
        ends <- trans[trans$group==2, ,drop=FALSE]
        if (outward){
            curvature <- unlist(mapply(generate_curvature2, starttheta=starts$theta,
                                       endtheta=ends$theta, hratio=starts$hratio, ncp=starts$ncp,
                                       SIMPLIFY=FALSE))
        }else{
            curvature <- unlist(mapply(generate_curvature, starttheta=starts$theta, 
                                       endtheta=ends$theta, hratio=starts$hratio, ncp=starts$ncp, 
                                       SIMPLIFY=FALSE))
        }
        ends <- rename(subset(ends, select=c(x, y)), c("xend"="x", "yend"="y"))
        trans <- cbind(starts, ends)
        trans$group <- tmpgroup
        trans$curvature <- curvature
    }else{
        trans <- coord$transform(data, panel_params)
    }
    arrow.fill <- arrow.fill %|||% trans$colour

    grobs <- lapply(seq_len(nrow(trans)), function(i){
                        curveGrob(
                              trans$x[i], trans$y[i], trans$xend[i], trans$yend[i],
                              default.units = "native",
                              curvature = trans$curvature[i], angle = angle, ncp = trans$ncp[i],
                              square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
                              gp = gpar(col = alpha(trans$colour[i], trans$alpha[i]),
                                        fill = alpha(arrow.fill[i], trans$alpha[i]),
                                        lwd = trans$size[i] * .pt,
                                        lty = trans$linetype[i],
                                        lineend = lineend),
                              arrow = arrow,
                              shape = shape)})
    class(grobs) <- "gList"
    return(ggname("geom_curvelink", gTree(children=grobs)))
  }
)

# for inward curve lines
generate_curvature <- function(starttheta, endtheta, hratio, ncp){
    flag <- endtheta - starttheta
    newflag <- min(c(abs(flag), 2*pi-abs(flag)))
    if (flag > 0){
        if (flag <= pi){
            origin_direction <- 1
        }else{
            origin_direction <- -1
        }
    }else{
        if (abs(flag)<=pi){
            origin_direction <- -1
        }else{
            origin_direction <- 1
        }
    }
    curvature <- hratio * origin_direction * (1 - newflag/pi)
    return(curvature)
}

# for outward curve lines
generate_curvature2 <- function(starttheta, endtheta, hratio, ncp){
    flag <- endtheta - starttheta
    newflag <- min(c(abs(flag), 2*pi-abs(flag)))
    if (flag > 0){
        if (flag <= pi){
            origin_direction <- -1
        }else{
            origin_direction <- 1
        }
    }else{
        if (abs(flag)<=pi){
            origin_direction <- 1
        }else{
            origin_direction <- -1
        }
    }
    if (newflag>pi/2){
        curvature <- hratio * origin_direction * pi/newflag
    }else{
        curvature <- hratio * origin_direction * (1-newflag/pi)
    }
    return (curvature)
}

#' @importFrom utils getFromNamespace
ggname <- getFromNamespace("ggname", "ggplot2")

"%|||%" <- function(x, y){
    if (is.null(x)){
        return(y)
    }
    if (is.null(y)) {
        return(x)
    }

    if (length(x)<length(y)) {
        return (y)
    } else {
        return (x)
    }
}    
