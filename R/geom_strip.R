##' annotate associated taxa (from taxa1 to taxa2, can be Monophyletic, Polyphyletic or Paraphyletc Taxa) with bar and (optional) text label
##'
##'
##' @title geom_strip
##' @param taxa1 taxa1
##' @param taxa2 taxa2
##' @param label add label alongside the bar (optional)
##' @param offset offset of bar and text from the clade
##' @param offset.text offset of text from bar
##' @param align logical, whether to align bars to the most distant bar ,defaults to "TRUE"
##' Note that if "FALSE", the bars might cross the tree
##' @param barsize set size of the bar
##' @param extend extend bar length vertically
##' @param fontsize set size of the text
##' @param angle set the angle of text
##' @param geom one of 'text' or 'label'
##' @param hjust adjust the horizonal position of the bar
##' @param color set color for bar and label
##' @param fill set color to fill label background, only work with geom='label'
##' @param family "sans" by default, can be any supported font
##' @param parse logical, whether to parse labels, if "TRUE", the labels will be parsed into expressions, defaults to "FALSE"
##' @param ... additional parameter
##' @return ggplot layers
##' @export
##' @author Guangchuang Yu
##' @examples
##' library(ggtree)
##' tr<- rtree(15)
##' x <- ggtree(tr)
##' x + geom_strip(13, 1, color = "red") + geom_strip(3, 7, color = "blue")
##' @references
##' For more detailed demonstration of this function, please refer to chapter 5.2.1 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_strip <- function(taxa1, taxa2, label, offset=0, offset.text=0,
                       align=TRUE, barsize=0.5, extend=0, fontsize=3.88,
                       angle=0, geom="text", hjust=0, color = 'black', fill=NA, family="sans",
                       parse=FALSE, ...) {

    if (missing(label)) label <- NA

    structure(list(taxa1 = taxa1,
                   taxa2 = taxa2,
                   label = label,
                   offset = offset,
                   offset.text = offset.text,
                   align = align,
                   barsize = barsize,
                   extend = extend,
                   fontsize = fontsize,
                   angle = angle,
                   geom = geom,
                   hjust = hjust,
                   color = color,
                   fill = fill,
                   family = family,
                   parse = parse,
                   params = list(...)),
              class = "striplabel")
}


## geom_strip <- function(taxa1, taxa2, label, offset=0, offset.text=0,
##                            align=TRUE, barsize=0.5, extend=0, fontsize=3.88,
##                            angle=0, geom="text", hjust=0, fill=NA, family="sans",
##                            parse=FALSE, ...) {

##     mapping <- aes_(x=~x, y=~y, node=~node, label = ~label, xend=~x, yend=~y)
##     data <- NULL
##     position <- "identity"
##     show.legend <- NA
##     na.rm <- TRUE
##     inherit.aes <- FALSE

##     layer_bar <- stat_stripBar(taxa1=taxa1, taxa2=taxa2, offset=offset, align=align,
##                                size=barsize, barextend=extend,
##                                mapping=mapping, data=data,
##                                position=position, show.legend = show.legend,
##                                inherit.aes = inherit.aes, na.rm=na.rm, ...)

##     if (missing(label) || is.na(label) || is.null(label)) {
##         return(layer_bar)
##     }

##     if (geom == "text") {
##         ## no fill parameter
##         layer_text <- stat_stripText(taxa1=taxa1, taxa2=taxa2, label=label, offset=offset+offset.text,
##                                      align=align, size=fontsize, barextend=extend, angle=angle, family=family,
##                                      mapping=mapping, data=data, geom=geom, hjust=hjust,
##                                      position=position, show.legend = show.legend,
##                                      inherit.aes = inherit.aes, na.rm=na.rm, parse=parse, ...)

##     } else {
##         layer_text <- stat_stripText(taxa1=taxa1, taxa2=taxa2, label=label, offset=offset+offset.text,
##                                      align=align, size=fontsize, barextend=extend, angle=angle,
##                                      fill=fill,family=family,
##                                      mapping=mapping, data=data, geom=geom, hjust=hjust,
##                                      position=position, show.legend = show.legend,
##                                      inherit.aes = inherit.aes, na.rm=na.rm, parse=parse, ...)
##     }

##     list(
##         layer_bar,
##         layer_text
##     )
## }


## stat_stripText <- function(mapping=NULL, data=NULL,
##                            geom="text", position="identity",
##                            taxa1, taxa2, label, offset, align, barextend, ...,
##                            show.legend=NA, inherit.aes=FALSE, na.rm=FALSE, parse=FALSE) {

##     layer(stat=StatStripText,
##           data=data,
##           mapping=mapping,
##           geom=geom,
##           position=position,
##           show.legend = show.legend,
##           inherit.aes = inherit.aes,
##           params=list(taxa1=taxa1,
##                       taxa2=taxa2,
##                       label=label,
##                       offset=offset,
##                       align=align,
##                       barextend=barextend,
##                       na.rm=na.rm,
##                       parse=parse,
##                       ...),
##           check.aes = FALSE
##           )

## }

## stat_stripBar <- function(mapping=NULL, data=NULL,
##                           geom="segment", position="identity",
##                           taxa1, taxa2, offset, align, barextend, ...,
##                           show.legend=NA, inherit.aes=FALSE, na.rm=FALSE) {

##     layer(stat=StatStripBar,
##           data=data,
##           mapping=mapping,
##           geom=geom,
##           position=position,
##           show.legend = show.legend,
##           inherit.aes = inherit.aes,
##           params=list(taxa1=taxa1,
##                       taxa2=taxa2,
##                       offset=offset,
##                       align=align,
##                       barextend=barextend,
##                       na.rm=na.rm,
##                       ...),
##           check.aes = FALSE
##           )

## }

## StatStripText <- ggproto("StatStripText", Stat,
##                          compute_group = function(self, data, scales, params, taxa1, taxa2,
##                                                   label, offset, align, barextend) {
##                              print('text' )
##                              print(data )
##                              df <- get_striplabel_position(data, taxa1, taxa2, offset, align,
##                                                            barextend, adjustRatio = 1.03)
##                              df$y <- mean(c(df$y, df$yend))
##                              df$label <- label
##                              return(df)
##                          },
##                          required_aes = c("x", "y", "label")
##                          )



## StatStripBar <- ggproto("StatStripBar", Stat,
##                         compute_group = function(self, data, scales, params,
##                                                  taxa1, taxa2, offset, align, barextend) {
##                             print('bar' )
##                             print(data )
##                             get_striplabel_position(data, taxa1, taxa2, offset,
##                                                     align, barextend, adjustRatio=1.02)
##                         },
##                         required_aes = c("x", "y", "xend", "yend")
##                         )


get_striplabel_position <- function(data, taxa1, taxa2, offset, align, barextend, adjustRatio) {
    df <- get_striplabel_position_(data, taxa1, taxa2, barextend)
    if (align) {
        mx <- max(data$x, na.rm=TRUE)
    } else {
        mx <- df$x
    }
    mx <- mx * adjustRatio + offset
    data.frame(x=mx, xend=mx, y=df$y, yend=df$yend)
}


get_striplabel_position_ <- function(data, taxa1, taxa2, barextend=0) {
    if (length(barextend) == 1) {
        barextend <- rep(barextend, 2)
    }

    node1 <- taxa2node(data, taxa1)
    node2 <- taxa2node(data, taxa2)

    xx <- with(data, c(x[node == node1], x[node == node2]))
    yy <- with(data, c(y[node == node1], y[node == node2]))

    data.frame(x=max(xx), y=min(yy)-barextend[2], yend=max(yy)+barextend[1])
}

## used in geom_strip, geom_taxalink
taxa2node <- function(data, taxa) {
    ## if (! 'label' %in% colnames(data))
    ##     data$label <- NA

    ## idx <- which(taxa == data$label | taxa == data$node)

    ## if (length(idx) == 0) {
    ##     print(taxa )
    ##     print(data )
    ##     stop("input taxa is not valid...")
    ## }

    ## return(data$node[idx])
    if (is.numeric(taxa))
        return(taxa)

    nodeid.tbl_tree(data, taxa)
}

