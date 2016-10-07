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
##' @param colnames_angle angle of column names
##' @param colnames_level levels of colnames
##' @param colnames_offset_x x offset for column names
##' @param colnames_offset_y y offset for column names
##' @param font.size font size of matrix colnames
##' @param hjust hjust for column names (0: align left, 0.5: align center, 1: align righ)
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
                     colnames=TRUE, colnames_position="bottom", colnames_angle=0, colnames_level=NULL,
                     colnames_offset_x = 0, colnames_offset_y = 0, font.size=4, hjust=0.5) {
    
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
    
    dd <- as.data.frame(data)
    ## dd$lab <- rownames(dd)
    lab <- df$label[order(df$y)]
    dd <- dd[lab, , drop=FALSE]
    dd$y <- sort(df$y)
    dd$lab <- lab
    ## dd <- melt(dd, id=c("lab", "y"))
    dd <- gather(dd, variable, value, -c(lab, y))

    i <- which(dd$value == "")
    if (length(i) > 0) {
        dd$value[i] <- NA
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
    dd$width <- width
    
    if (is.null(color)) {
        p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, inherit.aes=FALSE)
    } else {
        p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, color=color, inherit.aes=FALSE)
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
        mapping$y <- y
        p2 <- p2 + geom_text(data=mapping, aes(x=to, y = y, label=from), size=font.size, inherit.aes = FALSE,
                             angle=colnames_angle, nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
    }
    
    p2 <- p2 + theme(legend.position="right", legend.title=element_blank())
    ## p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))
    
    attr(p2, "mapping") <- mapping
    return(p2)
}

