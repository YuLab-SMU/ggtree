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

#' annotate associated taxa (from taxa1 to taxa2, can be Monophyletic, Polyphyletic or Paraphyletc Taxa) with bar and (optional) text label or image
#'
#' @title geom_striplab
#' @param taxa1 can be label or node number
#' @param taxa2 can be label or node number
#' @param label character, character to be showed, when data and mapping is NULL, it is required.
#' @param data data.frame, the data to be displayed in the annotation, default is NULL.
#' @param mapping Set of aesthetic mappings, default is NULL. The detail see the following explanation.
#' @param geom character, one of 'text', 'label', 'shadowtext', 'image' and 'phylopic',
#' default is 'text', and the parameter see the Aesthetics For Specified Geom.
#' @param parse logical, whether parse label to emoji font, default is FALSE.
#' @param ... additional parameters, see also following section.
#'
#' additional parameters can refer the following parameters.                                                                                                                                                      ##'     \itemize{
#'        \item \code{offset} distance bar and tree, offset of bar and text from
#'         the clade, default is 0.
#'        \item \code{offset.text} distance bar and text, offset of text from bar,
#'         default is 0.
#'        \item \code{align} logical, whether align clade lab, default is FALSE.
#'        \item \code{extend} numeric, extend the length of bar, default is 0.
#'        \item \code{angle} numeric or 'auto', if angle is auto, the angle of text will
#'         be calculated automatically, which is useful for the circular etc layout, default is 0.
#'        \item \code{horizontal} logical, whether set label to horizontal, default is TRUE.
#'        \item \code{barsize} the width of line, default is 0.5.
#'        \item \code{barcolour} the colour of line, default is 'black'.
#'        \item \code{fontsize} the size of text, default is 3.88.
#'        \item \code{textcolour} the colour of text, default is 'black'.
#'        \item \code{imagesize} the size of image, default is 0.05.
#'        \item \code{imagecolor} the colour of image, default is NULL, when
#'        geom="phylopic", it should be required.
#'     }
#' The parameters also can be set in mapping, when data is provided. Note: the barsize, barcolour,
#' fontsize, textcolour, imagesize and imagecolor should not be set in mapping (aesthetics). When
#' the color and size are not be set in mapping, user can modify them to adjust the attributes of
#' specified geom.
#'
#' @section Aesthetics For Specified Geom:
#' \code{geom_striplab()} understands the following aesthetics for geom="text"(required
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{taxa1}} selected tip label or tip node, it is required.
#'        \item \strong{\code{taxa2}} selected another tip label or tip node, it is required.
#'        \item \strong{\code{label}} labels to be shown, it is required.
#'        \item \code{colour} the colour of text, default is "black".
#'        \item \code{size} the size of text, default is 3.88.
#'        \item \code{angle} the angle of text, default is 0.
#'        \item \code{hjust} A numeric vector specifying horizontal justification, default is 0.
#'        \item \code{vjust} A numeric vector specifying vertical justification, default is 0.5.
#'        \item \code{alpha} the transparency of text, default is NA.
#'        \item \code{family} the family of text, default is 'sans'.
#'        \item \code{fontface} the font face of text, default is 1 (plain), others are
#'         2 (bold), 3 (italic), 4 (bold.italic).
#'        \item \code{lineheight} The height of a line as a multiple of the size of text, default is 1.2 .
#'     }
#'  when the colour, size are not be set in mapping, and user want to modify the colour of text,
#'  they should use textcolour, fontsize to avoid the confusion with bar layer annotation.
#'
#' \code{geom_striplab()} understands the following aesthethics for geom="label" (required
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{taxa1}} selected node to hight light, it is required.
#'        \item \strong{\code{taxa2}} selected another tip label or tip node, it is required.
#'        \item \strong{\code{label}} labels to be shown, it is required.
#'        \item \code{colour} the colour of text, default is "black".
#'        \item \code{fill} the background colour of the label, default is "white".
#'        \item \code{size} the size of text, default is 3.88.
#'        \item \code{angle} the angle of text, default is 0.
#'        \item \code{hjust} A numeric vector specifying horizontal justification, default is 0.
#'        \item \code{vjust} A numeric vector specifying vertical justification, default is 0.5.
#'        \item \code{alpha} the transparency of text, default is NA.
#'        \item \code{family} the family of text, default is 'sans'.
#'        \item \code{fontface} the font face of text, default is 1 (plain), others are
#'         2 (bold), 3 (italic), 4 (bold.italic).
#'        \item \code{lineheight} The height of a line as a multiple of the size of text, default is 1.2 .
#'     }
#'  when the colour, size are not be set in mapping, and user want to modify the colour of text,
#'  they should use textcolour, fontsize to avoid the confusion with bar layer annotation.
#'
#' \code{geom_striplab()} understands the following aesthethics for geom="shadowtext" (required
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{taxa1}} selected node to hight light, it is required.
#'        \item \strong{\code{taxa2}} selected another tip label or tip node, it is required.
#'        \item \strong{\code{label}} labels to be shown, it is required.
#'        \item \code{colour} the colour of text, default is "black".
#'        \item \code{bg.colour} the background colour of text, default is 'black'.
#'        \item \code{bg.r} the width of background text, default is 0.1.
#'        \item \code{size} the size of text, default is 3.88.
#'        \item \code{angle} the angle of text, default is 0.
#'        \item \code{hjust} A numeric vector specifying horizontal justification, default is 0.
#'        \item \code{vjust} A numeric vector specifying vertical justification, default is 0.5.
#'        \item \code{alpha} the transparency of text, default is NA.
#'        \item \code{family} the family of text, default is 'sans'.
#'        \item \code{fontface} the font face of text, default is 1 (plain), others are
#'         2 (bold), 3 (italic), 4 (bold.italic).
#'        \item \code{lineheight} The height of a line as a multiple of the size of text, default is 1.2 .
#'     }
#'  when the colour, size are not be set in mapping, and user want to modify the colour of text,
#'  they should use textcolour, fontsize to avoid the confusion with bar layer annotation.
#'
#' \code{geom_striplab()} understands the following aesthethics for geom="image" or geom="phylopic" (required
#' aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{taxa1}} selected node to hight light, it is required.
#'        \item \strong{\code{taxa2}} selected another tip label or tip node, it is required.
#'        \item \strong{\code{label}} labels to be shown, it is required.
#'        \item \strong{\code{image}} the image to be annotated, when geom="phylopic",
#'         the uid of phylopic databases, it is required.
#'        \item \code{colour} the color of image, default is NULL.
#'        \item \code{size} the size of image, default is 0.05.
#'        \item \code{alpha} the alpha of image, default is 0.8.
#'     }
#'  when the colour, size are not be set in mapping, and user want to modify the colour of image,
#'  they should use imagecolour, imagesize to avoid the confusion with bar layer annotation.
#' @export
#' @examples
#' set.seed(123)
#' tr <- rtree(10)
#' dt <- data.frame(ta1=c("t5", "t1"), ta2=c("t6", "t3"), group=c("A", "B"))
#' p <- ggtree(tr) + geom_tiplab()
#' p2 <- p + 
#'       geom_striplab(
#'         data = dt,
#'         mapping = aes(taxa1 = ta1, taxa2 = ta2, 
#'                       label = group, color=group),
#'         align = TRUE,
#'         show.legend = FALSE
#'       )
#' p2
geom_striplab <- function(
               taxa1 = NULL, 
               taxa2 = NULL,
               label = NULL,
               data = NULL,
               mapping = NULL,
               geom = "text",
               parse = FALSE,
               ...
    ){

    params <- list(inherit.aes = FALSE, ...)
    structure(
       list(
         data = data, 
         mapping = mapping, 
         taxa1 = taxa1,
         taxa2 = taxa2,
         label = label, 
         geom = geom, 
         parse = parse, 
         params = params
       ),
       class = "striplab")

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

get_striplab_position <- function(data, taxa1, taxa2, offset, angle="auto", 
                                  align = TRUE, extend = 0, adjustRatio = 1.02, 
                                  horizontal = TRUE){
    df <- get_striplab_position_(data = data, taxa1 = taxa1, taxa2 = taxa2, 
                                 angle = angle, extend = extend, horizontal = horizontal)
    if (align){
        mx <- max(data$x, na.rm = TRUE)
    }else{
        mx <- df$x
    }
    angle <- df$angle
    mx <- mx * adjustRatio + offset
    data.frame(x = mx, xend = mx, y = df$y, yend=df$yend, angle = angle)
}

get_striplab_position_ <- function(data, taxa1, taxa2, angle, extend = 0, horizontal){
    if (length(extend) == 1) {
        extend <- rep(extend, 2)
    }

    node1 <- taxa2node(data, taxa1)
    node2 <- taxa2node(data, taxa2)

    xx <- with(data, c(x[node == node1], x[node == node2]))
    yy <- with(data, c(y[node == node1], y[node == node2]))

    d <- data.frame(x=max(xx), y=min(yy)-extend[2], yend=max(yy)+extend[1])
    if (missing(angle))
        return(d)
    if (angle == "auto") {
        anglerange <- with(data, c(angle[node == node1], angle[node == node2]))
        d$angle <- mean(anglerange)
        d$angle <- adjust_cladelabel_angle(angle=d$angle, horizontal=horizontal)
    } else {
        d$angle <- angle
    }
    return(d)    
}

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

