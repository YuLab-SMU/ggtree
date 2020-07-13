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
##' @param ... additional parameter
##' @return ggplot layer
##' @export
##' @author Guangchuang Yu
geom_taxalink <- function(taxa1, taxa2, curvature=0.5, arrow = NULL, arrow.fill = NULL, ...) {
    position = "identity"
    show.legend = NA
    na.rm = TRUE
    inherit.aes = FALSE

    mapping <- aes_(x=~x, y=~y, node=~node, label=~label, xend=~x, yend=~y)

    layer(stat=StatTaxalink,
          mapping=mapping,
          data = NULL,
          geom=GeomCurveLink,
          position='identity',
          show.legend=show.legend,
          inherit.aes = inherit.aes,
          params = list(taxa1 = taxa1,
                        taxa2 = taxa2,
                        curvature = curvature,
                        na.rm = na.rm,
                        arrow = arrow,
                        arrow.fill = arrow.fill,
                        ...),
          check.aes = FALSE
          )
}

StatTaxalink <- ggproto("StatTaxalink", Stat,
                        compute_group = function(self, data, scales, params, taxa1, taxa2) {
                            node1 <- taxa2node(data, taxa1)
                            node2 <- taxa2node(data, taxa2)
                           
                            x <- data$x
                            y <- data$y

                            data.frame(x = x[node1],
                                       xend = x[node2],
                                       y = y[node1],
                                       yend = y[node2])

                        },
                        required_aes = c("x", "y", "xend", "yend")
                        )

#' @importFrom ggplot2 GeomSegment
#' @importFrom grid gTree curveGrob gpar
GeomCurveLink <- ggproto("GeomCurveLink", GeomSegment,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, curvature=0.5),
  draw_panel = function(data, panel_params, coord, angle = 90,
                        ncp = 5, arrow = NULL, arrow.fill=NULL, lineend = "butt", na.rm = FALSE) {

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
        curvature <- unlist(mapply(generate_curvature, starttheta=starts$theta, endtheta=ends$theta, SIMPLIFY=FALSE))
        ends <- rename(subset(ends, select=c(x, y)), c("xend"="x", "yend"="y"))
        trans <- cbind(starts, ends)
        trans$group <- tmpgroup
        trans$curvature <- curvature
    }else{
        trans <- coord$transform(data, panel_params)
    }
    arrow.fill <- arrow.fill %||% trans$colour

    grobs <- lapply(seq_len(nrow(trans)), function(i){
                        curveGrob(
                              trans$x[i], trans$y[i], trans$xend[i], trans$yend[i],
                              default.units = "native",
                              curvature = trans$curvature[i], angle = angle, ncp = ncp,
                              square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
                              gp = gpar(col = alpha(trans$colour[i], trans$alpha[i]),
                                        fill = alpha(arrow.fill[i], trans$alpha[i]),
                                        lwd = trans$size[i] * .pt,
                                        lty = trans$linetype[i],
                                        lineend = lineend),
                                        arrow = arrow)})
    class(grobs) <- "gList"
    return(ggname("geom_curve_link", gTree(children=grobs)))
  }
)

generate_curvature <- function(starttheta, endtheta){
    flag <- endtheta - starttheta
    if (flag > 0){
        if (flag < pi){
            origin_direction <- 1
        }else{
            origin_direction <- -1
        }
    }else{
        if (abs(flag) < pi){
            origin_direction <- - 1
        }else{
            origin_direction <- 1
        }
    }
    flag <- min(c(abs(flag), 2*pi-abs(flag)))
    curvature <- origin_direction * (1 - flag/pi)
    return(curvature)
}

#' @importFrom utils getFromNamespace
"%||%" <- getFromNamespace("%||%", "ggplot2")

ggname <- getFromNamespace("ggname", "ggplot2")
