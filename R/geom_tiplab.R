##' add tip label layer
##'
##'
##' @title geom_tiplab
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param offset tiplab offset
##' @param align align tip lab or not, logical
##' @param linetype linetype for adding line if align = TRUE
##' @param linesize line size of line if align = TRUE
##' @param geom one of 'text' and 'label'
##' @param ... additional parameter
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab()
geom_tiplab <- function(mapping=NULL, hjust = 0,  align = FALSE, linetype = "dotted", linesize=1, geom="text", offset = 0, ...) {
    geom <- match.arg(geom, c("text", "label"))
    if (geom == "text") {
        text_geom <- geom_text2
    } else {
        text_geom <- geom_label2
    }
    x <- y <- label <- isTip <- NULL
    if (align == TRUE) {
        self_mapping <- aes(x = max(x, na.rm=TRUE) + diff(range(x, na.rm=TRUE))/200, y = y, label = label, subset= isTip)
    }
    else {
        self_mapping <- aes(x = x + diff(range(x, na.rm=TRUE))/200, y= y, label = label, subset= isTip)
    }

    if (is.null(mapping)) {
        text_mapping <- self_mapping
    } else {
        text_mapping <- modifyList(self_mapping, mapping)
    }


    show_segment <- FALSE
    if (align && (!is.na(linetype) && !is.null(linetype))) {
        show_segment <- TRUE
    }

    list(
        text_geom(mapping=text_mapping,
                  hjust = hjust, nudge_x = offset, ...)
        ,
        if (show_segment)
            geom_tipsegment(mapping = aes(subset=isTip),
                            offset = offset,
                            linetype = linetype,
                            size = linesize, ...)
    )
}


##' add tip label for circular layout
##'
##'
##' @title geom_tiplab2
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param ... additional parameter, see geom_tiplab
##' @return tip label layer
##' @export
##' @author Guangchuang Yu
##' @references \url{https://groups.google.com/forum/#!topic/bioc-ggtree/o35PV3iHO-0}
geom_tiplab2 <- function(mapping=NULL, hjust=0, ...) {

    angle <- NULL
    isTip <- NULL
    ## m1 <- aes(subset=(abs(angle) < 90), angle=angle)
    ## m2 <- aes(subset=(abs(angle) >= 90), angle=angle+180)
    m1 <- aes(subset=(isTip & (angle < 90 | angle > 270)), angle=angle)
    m2 <- aes(subset=(isTip & (angle >= 90 & angle <=270)), angle=angle+180)

    if (!is.null(mapping)) {
        m1 <- modifyList(mapping, m1)
        m2 <- modifyList(mapping, m2)
    }

    list(geom_tiplab(m1, hjust=hjust, ...),
         geom_tiplab(m2, hjust=1-hjust, ...)
         )
}

geom_tipsegment <- function(mapping=NULL, data=NULL,
                            geom=GeomSegmentGGtree, position = "identity",
                            offset,  ...,
                            show.legend=NA, inherit.aes=FALSE,
                            na.rm=TRUE) {

    default_aes <- aes_(x=~x, y=~y)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(default_aes, mapping)
    }

    layer(stat=StatTipSegment,
          data = data,
          mapping = mapping,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(offset = offset,
                        na.rm = na.rm,
                        ...),
          if (packageVersion('ggplot2') > '2.1.0') check.aes = FALSE
          )
}

StatTipSegment <- ggproto("StatTipSegment", Stat,
                        compute_group = function(self, data, scales, params, offset) {
                            get_tipsegment_position(data, offset)
                        },
                        required_aes = c("x", "y")
                        )


get_tipsegment_position <- function(data, offset, adjustRatio=1/200) {
    adjust <- diff(range(data$x, na.rm=TRUE)) * adjustRatio
    xend <- data$x + adjust
    x <- max(data$x, na.rm = TRUE)  + offset
    y <- data$y
    data.frame(x=x, xend=xend, y=y, yend=y)
}


