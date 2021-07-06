##' add tip label layer
##'
##'
##' @title geom_tiplab
##' @param mapping aes mapping
##' @param hjust horizontal adjustment
##' @param offset tiplab offset, horizontal 
##' adjustment to nudge tip labels, default is 0.
##' @param align align tip lab or not, logical
##' @param linetype linetype for adding line if align = TRUE
##' @param linesize line size of line if align = TRUE
##' @param geom one of 'text', 'label', 'shadowtext', 'image' and 'phylopic'
##' @param as_ylab display tip labels as y-axis label, only works for rectangular and dendrogram layouts
##' @param ... additional parameter
##'
##' additional parameters can refer the following parameters. 
##'
##' The following parameters for geom="text".
##' \itemize{
##'     \item \code{size} control the size of tip labels, default is 3.88.
##'     \item \code{colour} control the colour of tip labels, default is "black".
##'     \item \code{angle} control the angle of tip labels, default is 0.
##'     \item \code{vjust} A numeric vector specifying vertical justification, default is 0.5.
##'     \item \code{alpha} the transparency of text, default is NA.
##'     \item \code{family} the family of text, default is 'sans'.
##'     \item \code{fontface} the font face of text, default is 1 (plain), others are 
##'      2 (bold), 3 (italic), 4 (bold.italic).
##'     \item \code{lineheight} The height of a line as a multiple of the size of text, default is 1.2 .
##'     \item \code{nudge_x} horizontal adjustment to nudge labels, default is 0. 
##'     \item \code{nudge_y}  vertical adjustment to nudge labels, default is 0.
##'     \item \code{check.overlap} if TRUE, text that overlaps previous text in the same layer 
##'      will not be plotted.
##'     \item \code{parse} if TRUE, the labels will be parsed into expressions, if it is 'emoji', the labels
##'      will be parsed into emojifont.
##' }
##'
##' The following parameters for geom="label".
##' \itemize{
##'     \item \code{size} the size of tip labels, default is 3.88.
##'     \item \code{colour} the colour of tip labels, default is "black".
##'     \item \code{fill} the colour of rectangular box of labels, default is "white".
##'     \item \code{vjust} numeric vector specifying vertical justification, default is 0.5.
##'     \item \code{alpha} the transparency of labels, default is NA.
##'     \item \code{family} the family of text, default is 'sans'.
##'     \item \code{fontface} the font face of text, default is 1 (plain), others are
##'     2 (bold), 3 (italic), 4 (bold.italic).
##'     \item \code{lineheight} The height of a line as a multiple of the size of text, default is 1.2.
##'     \item \code{nudge_x} horizontal adjustment to nudge labels, default is 0.
##'     \item \code{nudge_y}  vertical adjustment, default is 0.
##'     \item \code{check.overlap} if TRUE, text that overlaps previous text in the same layer
##'      will not be plotted.
##'     \item \code{parse} if TRUE, the labels will be parsed into expressions, if it is 'emoji', the labels
##'      will be parsed into emojifont.
##'     \item \code{label.padding} Amount of padding around label, default is 'unit(0.25, "lines")'.
##'     \item \code{label.r} Radius of rounded corners, default is 'unit(0.15, "lines")'.
##'     \item \code{label.size} Size of label border, in mm, default is 0.25.
##' }
##'
##' The following parameters for geom="shadowtext", some parameters are like to geom="text".
##' \itemize{
##'     \item \code{bg.colour} the background colour of text, default is "black".
##'     \item \code{bg.r} the width of background of text, default is 0.1 .
##' }
##'
##' The following parameters for geom="image" or geom="phylopic".
##' \itemize{
##'     \item \code{image} the image file path for geom='image', but when geom='phylopic',
##'      it should be the uid of phylopic databases.
##'     \item \code{size} the image size, default is 0.05.
##'     \item \code{colour} the color of image, default is NULL.
##'     \item \code{alpha} the transparency of image, default is 0.8.
##' }
##'
##' The following parameters for the line when align = TRUE.
##' \itemize{
##'     \item \code{colour} the colour of line, default is 'black'.
##'     \item \code{alpha} the transparency of line, default is NA.
##'     \item \code{arrow} specification for arrow heads, 
##'     as created by arrow(), default is NULL.
##'     \item \code{arrow.fill} fill color to usse for the arrow head (if closed), 
##'     default is 'NULL', meaning use 'colour' aesthetic.
##' }
##' @return tip label layer
##' @importFrom ggplot2 geom_text
##' @importFrom utils modifyList
##' @export
##' @author Guangchuang Yu
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + geom_tiplab()
geom_tiplab <- function(mapping=NULL, hjust = 0,  align = FALSE, linetype = "dotted",
                        linesize=0.5, geom="text",  offset=0, as_ylab = FALSE, ...) {
    structure(list(mapping = mapping,
                   hjust = hjust,
                   align = align,
                   linetype = linetype,
                   linesize = linesize,
                   geom = geom,
                   offset = offset,
                   as_ylab = as_ylab,
				   node = "external",
                   ...),
              class = "tiplab")
}

geom_tiplab_as_ylab <- function(hjust = 0, position = "right", ...) {
    structure(list(hjust = hjust,
                   position = position,
                   ...),
              class =  "tiplab_ylab"
              )
}

geom_tiplab_rectangular <- function(mapping=NULL, hjust = 0,  align = FALSE, 
                                    linetype = "dotted", linesize=0.5, geom="text",  
                                    offset=0, #family = "", fontface = "plain", 
                                    node="external", ...) {
    params <- list(...)
    if ("nudge_x" %in% names(params)){
        if (offset != 0){
            warning_wrap("Both nudge_x and offset arguments are provided.
                         Because they all adjust the horizontal offset of labels,
                         and the 'nudge_x' is consistent with 'ggplot2'. The
                         'offset' will be deprecated here and only the 'nudge_x' will be used.")
        }
        offset <- params$nudge_x
        params$nudge_x <- NULL
    }
    geom <- match.arg(geom, c("text", "label", "shadowtext", "image", "phylopic"))
    if (geom == "text") {
        label_geom <- geom_text2
    } else if (geom == "label") {
        label_geom <- geom_label2
    } else if (geom == 'shadowtext') {
        label_geom <- get_fun_from_pkg("shadowtext", "geom_shadowtext")
    } else if (geom == "image") {
        label_geom <- get_fun_from_pkg("ggimage", "geom_image")
    } else if (geom == "phylopic") {
        label_geom <- get_fun_from_pkg("ggimage", "geom_phylopic")
    }

    nodelab <- node
    x <- y <- label <- isTip <- node <- NULL
    if (align == TRUE) {
        self_mapping <- aes(x = max(x, na.rm=TRUE) + diff(range(x, na.rm=TRUE))/200, y = y,
                            label = label, node = node)#, subset = isTip)
    }
    else {
        self_mapping <- aes(x = x + diff(range(x, na.rm=TRUE))/200, y= y,
                            label = label, node = node)#, subset = isTip)
    }
    subset <- switch(nodelab,
					 internal = aes_string(subset="!isTip"),
					 external = aes_string(subset="isTip"),
					 all = aes_string(subset=NULL)
					 )
    self_mapping <- modifyList(self_mapping, subset)
    if (is.null(mapping)) {
        text_mapping <- self_mapping
    } else {
        if (!is.null(mapping$subset) && nodelab != "all"){
            newsubset <- aes_string(subset=paste0(as.expression(get_aes_var(mapping, "subset")), 
                                                  '&', 
                                                  as.expression(get_aes_var(subset, "subset")))
                                    )
            self_mapping <- modifyList(self_mapping, newsubset)
            mapping$subset <- NULL
        }
        text_mapping <- modifyList(self_mapping, mapping)
    }
    show_segment <- FALSE
    if (align && (!is.na(linetype) && !is.null(linetype))) {
        show_segment <- TRUE
        segment_mapping <- aes(x = max(x, na.rm=TRUE),
                               xend = x + diff(range(x, na.rm=TRUE))/200,
                               y = y, yend = y,
                               node = node,
                               label = label,
                               subset = isTip)
        if (!is.null(mapping))
            segment_mapping <- modifyList(segment_mapping, mapping)
    }
    imageparams <- list(mapping=text_mapping, hjust = hjust, nudge_x = offset, stat = StatTreeData)
    imageparams <- extract_params(imageparams, params, c("size", "alpha", "color", "colour", "image", 
                                                         "angle", "position", "inherit.aes", "by", "show.legend",
                                                         "image_fun", ".fun", "asp", "nudge_y", "height", "na.rm")) 
    labelparams <- list(mapping=text_mapping, hjust = hjust, nudge_x = offset, stat = StatTreeData)
    labelparams <- extract_params(labelparams, params, 
                                  c("size", "alpha", "vjust", "color", "colour", "angle", "alpha", "family", "fontface",
                                    "lineheight", "fill", "position", "nudge_y", "show.legend", "check_overlap",
                                    "parse", "inherit.aes", "na.rm", "label.r", "label.size", "label.padding",
                                    "bg.colour", "bg.r"))
    list(
        if (show_segment){
            lineparams <- list(mapping = segment_mapping, linetype=linetype, nudge_x = offset, size = linesize, stat = StatTreeData)
            lineparams <- extract_params(lineparams, params, c("colour", "alpha", "show.legend",  "na.rm",
                                                               "inherit.aes", "arrow", "arrow.fill", "lineend")) 
            do.call("geom_segment2", lineparams)
        }
       ,
        if (geom %in% c("image", "phylopic")) {
            do.call("label_geom", imageparams)
        } else {
            do.call("label_geom", labelparams)
        }
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
##' @references <https://groups.google.com/forum/#!topic/bioc-ggtree/o35PV3iHO-0>
##' @seealso [geom_tiplab]
geom_tiplab2 <- function(mapping=NULL, hjust=0, ...) {
    params <- list(...)
    #if ("nodelab" %in% names(params) && params[["nodelab"]]){
    #    # for geom_nodelab
    #    subset1 <- "(!isTip & (angle < 90 | angle > 270))"
    #    subset2 <- "(!isTip & (angle >= 90 & angle <= 270))"
    #}else{
    #    # for geom_tiplab
    #    subset1 <- "(isTip & (angle < 90 | angle > 270))"
    #    subset2 <- "(isTip & (angle >= 90 & angle <=270))"
    #}
    subset1 <- "(angle < 90 | angle > 270)"
    subset2 <- "(angle >= 90 & angle <=270)"
    m1 <- aes_string(subset=subset1, angle="angle", node = "node")
    m2 <- aes_string(subset=subset2, angle="angle+180", node = "node")

    if (!is.null(mapping)) {
        if (!is.null(mapping$subset)) {
            newsubset1 <- paste0(as.expression(get_aes_var(mapping, "subset")), '&', subset1)
            newsubset2 <- paste0(as.expression(get_aes_var(mapping, "subset")), '&', subset2)
            m1 <- aes_string(angle = "angle", node = "node", subset = newsubset1)
            m2 <- aes_string(angle = "angle+180", node = "node", subset = newsubset2)
        }
        m1 <- modifyList(mapping, m1)
        m2 <- modifyList(mapping, m2)
    }
    #params[["nodelab"]] <- NULL
    params1 <- params2 <- params
    params1[["mapping"]] <- m1
    params1[["hjust"]] <- hjust
    params2[["mapping"]] <- m2
    params2[["hjust"]] <- 1-hjust
    list(do.call("geom_tiplab_rectangular", params1),
         do.call("geom_tiplab_rectangular", params2)
         )
}

geom_tiplab_circular <- geom_tiplab2



#' Padding taxa labels
#'
#' This function add padding character to the left side of taxa labels.
#' @param label taxa label 
#' @param justify should a character vector be left-justified, right-justified (default), centred or left alone.
#' @param pad padding character (default is a dot)
#'
#' @return Taxa labels with padding characters added
#' @export
#' @author Guangchuang Yu and Yonghe Xia
#' @references <https://groups.google.com/g/bioc-ggtree/c/INJ0Nfkq3b0/m/lXefnfV5AQAJ>
#' @examples
#' library(ggtree)
#' set.seed(2015-12-21)
#' tree <- rtree(5)
#' tree$tip.label[2] <- "long string for test"
#' label_pad(tree$tip.label)
label_pad <- function(label, justify = "right", pad = "\u00B7") {
    x <- format(label, 
                width = max(nchar(label)),
                justify = justify)
    len <- vapply(gregexpr("^\\s+", x),
                  attr, "match.length",
                  FUN.VALUE = numeric(1))
    len[len<0] <- 0
    
    y <- vapply(len, 
                function(i) paste0(rep(pad, each=i), collapse = ''),
                FUN.VALUE = character(1))
    paste0(y, label)
}


extract_params <- function(originparam, inputparam, defaultparam){
    if (any(defaultparam %in% names(inputparam))){
        args <- intersect(defaultparam, names(inputparam))
        originparam <- c(originparam, inputparam[names(inputparam) %in% args])
    }
    
    return (originparam)

}
