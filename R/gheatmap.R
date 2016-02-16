##' append a heatmap of a matrix to right side of phylogenetic tree
##'
##' 
##' @title gheatmap
##' @param p tree view
##' @param data matrix or data.frame
##' @param offset offset of heatmap to tree
##' @param width total width of heatmap, compare to width of tree
##' @param low color of lowest value
##' @param high color of highest value
##' @param color color of heatmap cell border
##' @param colnames logical, add matrix colnames or not
##' @param colnames_position one of 'bottom' or 'top'
##' @param colnames_level levels of colnames
##' @param font.size font size of matrix colnames
##' @return tree view
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 guide_legend
##' @importFrom ggplot2 scale_fill_gradient
##' @importFrom ggplot2 scale_fill_discrete
##' @export
##' @author Guangchuang Yu
gheatmap <- function(p, data, offset=0, width=1, low="green", high="red", color="white",
                     colnames=TRUE, colnames_position="bottom", colnames_level=NULL, font.size=4) {

    colnames_position %<>% match.arg(c("bottom", "top"))
    variable <- value <- lab <- y <- NULL
    
    ## if (is.null(width)) {
    ##     width <- (p$data$x %>% range %>% diff)/30
    ## }

    ## convert width to width of each cell
    width <- width * (p$data$x %>% range %>% diff) / ncol(data)
    
    isTip <- x <- y <- variable <- value <- from <- to <- NULL
 
    df <- p$data
    df <- df[df$isTip,]
    start <- max(df$x) + offset

    dd <- data[df$label[order(df$y)],]
    dd$y <- sort(df$y)

    dd$lab <- rownames(dd)
    ## dd <- melt(dd, id=c("lab", "y"))
    dd <- gather(dd, variable, value, -c(lab, y))
    
    if (any(dd$value == "")) {
        dd$value[dd$value == ""] <- NA
    }
    if (is.null(colnames_level)) {
        dd$variable <- factor(dd$variable, levels=colnames(data))
    } else {
        dd$variable <- factor(dd$variable, levels=colnames_level)
    }
    V2 <- start + as.numeric(dd$variable) * width
    mapping <- data.frame(from=dd$variable, to=V2)
    mapping <- unique(mapping)

    dd$x <- V2

    if (is.null(color)) {
        p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), inherit.aes=FALSE)
    } else {
        p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), color=color, inherit.aes=FALSE)
    }
    if (is(dd$value,"numeric")) {
        p2 <- p2 + scale_fill_gradient(low=low, high=high, na.value="white")
    } else {
        p2 <- p2 + scale_fill_discrete(na.value="white")
    }
    
    if (colnames) {
        if (colnames_position == "bottom") {
            y <- 0
        } else {
            y <- max(p$data$y) + 1
        }
        p2 <- p2 + geom_text(data=mapping, aes(x=to, label=from), y=y, size=font.size, inherit.aes = FALSE)
    }

    p2 <- p2 + theme(legend.position="right", legend.title=element_blank())
    p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))
    
    attr(p2, "mapping") <- mapping
    return(p2)
}

