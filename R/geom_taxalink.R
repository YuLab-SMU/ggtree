##' link between taxa
##'
##'
##' @title geom_taxalink
##' @param taxa1 taxa1, can be label or node number
##' @param taxa2 taxa2, can be label or node number
##' @param curvature A numeric value giving the amount of curvature.
##' Negative values produce left-hand curves,
##' positive values produce right-hand curves, and zero produces a straight line.
##' @param ... additional parameter
##' @return ggplot layer
##' @export
##' @importFrom ggplot2 GeomCurve
##' @author Guangchuang Yu
geom_taxalink <- function(taxa1, taxa2, curvature=0.5, ...) {
    position = "identity"
    show.legend = NA
    na.rm = TRUE
    inherit.aes = FALSE

    mapping <- aes_(x=~x, y=~y, node=~node, label=~label, xend=~x, yend=~y)

    layer(stat=StatTaxalink,
          mapping=mapping,
          data = NULL,
          geom=GeomCurve,
          position='identity',
          show.legend=show.legend,
          inherit.aes = inherit.aes,
          params = list(taxa1 = taxa1,
                        taxa2 = taxa2,
                        curvature = curvature,
                        na.rm = na.rm,
                        ...),
          if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
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

