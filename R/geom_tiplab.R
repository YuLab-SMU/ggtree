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
##' @param geom one of 'text', 'label', 'shadowtext', 'image' and 'phylopic'
##' @param as_ylab display tip labels as y-axis label, only works for rectangular and dendrogram layouts
##' @param ... additional parameter
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
    #####in order to check whether it is geom_nodelab
    .call <- match.call(call = sys.call(sys.parent(1)))
    nodelab <- ifelse(as.list(.call)[[1]]=="geom_nodelab", TRUE, FALSE)
    #####
    structure(list(mapping = mapping,
                   hjust = hjust,
                   align = align,
                   linetype = linetype,
                   linesize = linesize,
                   geom = geom,
                   offset = offset,
                   as_ylab = as_ylab,
                   nodelab = nodelab,
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
                                    offset=0, family = "", fontface = "plain", ...) {
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


    x <- y <- label <- isTip <- node <- NULL
    if (align == TRUE) {
        self_mapping <- aes(x = max(x, na.rm=TRUE) + diff(range(x, na.rm=TRUE))/200, y = y,
                            label = label, node = node, subset = isTip)
    }
    else {
        self_mapping <- aes(x = x + diff(range(x, na.rm=TRUE))/200, y= y,
                            label = label, node = node, subset = isTip)
    }

    if (is.null(mapping)) {
        text_mapping <- self_mapping
    } else {
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

    list(
        if (show_segment)
            geom_segment2(mapping = segment_mapping,
                          linetype = linetype, nudge_x = offset,
                          size = linesize, stat = StatTreeData, ...)
       ,
        if (geom %in% c("image", "phylopic")) {
            label_geom(mapping=text_mapping,
                       hjust = hjust, nudge_x = offset, stat = StatTreeData, ...)            
        } else {
            label_geom(mapping=text_mapping,
                       hjust = hjust, nudge_x = offset, stat = StatTreeData, 
                       family = family, fontface = fontface, ...)
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
    if (params[["nodelab"]]){
        # for geom_nodelab
        subset1 <- "(!isTip & (angle < 90 | angle > 270))"
        subset2 <- "(!isTip & (angle >= 90 & angle <= 270))"
    }else{
        # for geom_tiplab
        subset1 <- "(isTip & (angle < 90 | angle > 270))"
        subset2 <- "(isTip & (angle >= 90 & angle <=270))"
    }
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
    params[["nodelab"]] <- NULL
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

