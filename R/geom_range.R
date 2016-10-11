##' bar of range (HPD, range etc) to present uncertainty of evolutionary inference
##'
##'
##' @title geom_range
##' @param range range, e.g. "height_0.95_HPD"
##' @param ... additional parameter, e.g. color, size, alpha
##' @return ggplot layer
##' @importFrom ggplot2 aes_string
##' @export
##' @author Guangchuang Yu
geom_range <- function(range="height_0.95_HPD", ...) {
    position = "identity"
    show.legend = NA
    na.rm = TRUE
    inherit.aes = FALSE

    default_aes <- aes_(x=~x, y=~y, xend=~x, yend=~y)

    mapping <- modifyList(default_aes, aes_string(branch.length="branch.length", label=range))

    layer(
        stat = StatRange,
        mapping = mapping,
        data = NULL,
        geom = GeomSegment,
        position = position,
        show.legend=show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...),
        if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
    )

}

StatRange <- ggproto("StatRange", Stat,
                     compute_group = function(self, data, scales, params) {
                         ## label is actually the range

                         df <- data[!is.na(data[,"label"]),]
                         rr <- gsub("\\[", "", df[,"label"])
                         rr <- gsub("\\]", "", rr)
                         rr2 <- strsplit(rr, split=',') %>% do.call('rbind', .)
                         rr2 <- matrix(as.numeric(rr2), ncol=2, byrow=FALSE)
                         rr2 <- rr2 + df$x - df$branch
                         data.frame(x = rr2[,1],
                                    xend = rr2[,2],
                                    y = df$y,
                                    yend = df$y)
                     },
                     required_aes = c("x", "y", "xend", "yend")
                     )


