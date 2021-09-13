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
##' @param family font of matrix colnames
##' @param hjust hjust for column names (0: align left, 0.5: align center, 1: align righ)
##' @param legend_title title of fill legend
##' @param custom_column_labels instead of the column names from a matrix, input a custom vector of column labels
##' @return tree view
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 guide_legend
##' @importFrom ggplot2 scale_fill_gradient
##' @importFrom ggplot2 scale_fill_discrete
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom dplyr filter
##' @importFrom dplyr select
##' @export
##' @author Guangchuang Yu
gheatmap <- function(p, data, offset=0, width=1, low="green", high="red", color="white",
                     colnames=TRUE, colnames_position="bottom", colnames_angle=0, colnames_level=NULL,
                     colnames_offset_x = 0, colnames_offset_y = 0, font.size=4, family="", hjust=0.5, legend_title = "value",
                     custom_column_labels = NULL) {

    colnames_position %<>% match.arg(c("bottom", "top"))
    variable <- value <- lab <- y <- NULL

    ## if (is.null(width)) {
    ##     width <- (p$data$x %>% range %>% diff)/30
    ## }

    ## convert width to width of each cell
    width <- width * (p$data$x %>% range(na.rm=TRUE) %>% diff) / ncol(data)

    isTip <- x <- y <- variable <- value <- from <- to <- NULL

    ## handle the display of heatmap on collapsed nodes
    ## https://github.com/GuangchuangYu/ggtree/issues/242
    ## extract data on leaves (& on collapsed internal nodes) 
    ## (the latter is extracted only when the input data has data on collapsed
    ## internal nodes)
    df <- p$data
    nodeCo <- intersect(df %>% filter(is.na(x)) %>% 
                         select(.data$parent, .data$node) %>% unlist(), 
                     df %>% filter(!is.na(x)) %>% 
                         select(.data$parent, .data$node) %>% unlist())
    labCo <- df %>% filter(.data$node %in% nodeCo) %>% 
        select(.data$label) %>% unlist()
    selCo <- intersect(labCo, rownames(data))
    isSel <- df$label %in% selCo
    
    df <- df[df$isTip | isSel, ]
    start <- max(df$x, na.rm=TRUE) + offset

    dd <- as.data.frame(data)
    ## dd$lab <- rownames(dd)
    i <- order(df$y)

    ## handle collapsed tree
    ## https://github.com/GuangchuangYu/ggtree/issues/137
    i <- i[!is.na(df$y[i])]

    lab <- df$label[i]
    ## dd <- dd[lab, , drop=FALSE]
    ## https://github.com/GuangchuangYu/ggtree/issues/182
    dd <- dd[match(lab, rownames(dd)), , drop = FALSE]


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
    dd[[".panel"]] <- factor("Tree")
    if (is.null(color)) {
        p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, inherit.aes=FALSE)
    } else {
        p2 <- p + geom_tile(data=dd, aes(x, y, fill=value), width=width, color=color, inherit.aes=FALSE)
    }
    if (is(dd$value,"numeric")) {
        p2 <- p2 + scale_fill_gradient(low=low, high=high, na.value=NA, name = legend_title) # "white")
    } else {
        p2 <- p2 + scale_fill_discrete(na.value=NA, name = legend_title) #"white")
    }

    if (colnames) {
        if (colnames_position == "bottom") {
            y <- 0
        } else {
            y <- max(p$data$y) + 1
        }
        mapping$y <- y
        mapping[[".panel"]] <- factor("Tree")
        # if custom column annotations are provided
        if (!is.null(custom_column_labels)) {
            # assess the type of input for the custom column annotation
            # either a vector or a named vector with positions for specific names
            if (is.null(names(custom_column_labels))) {
                if (length(custom_column_labels) > nrow(mapping)) {
                    warning("Input label vector has more elements than there are columns")
                    sprintf("Using the first %s elements as labels", nrow(mapping))
                    custom_column_labels <- custom_column_labels[1:nrow(mapping)]
                    mapping$custom_labels <- custom_column_labels
                 } else if (length(custom_column_labels) < nrow(mapping)){
                        warning("Input label vector has fewer elements than there are columns")
                        sprintf("Using all available labels, n = %s", length(custom_column_labels))
                     mapping$custom_labels <- c(custom_column_labels, rep("", nrow(mapping) - length(custom_column_labels)))
                 } else {
                        mapping$custom_labels <- custom_column_labels
                    }
                } else {
                vector_to_fill <- rep("", nrow(mapping))
                for (name in names(custom_column_labels)) {
                    if (!as.numeric(unname(custom_column_labels[name]))) {
                        warning("At least one element of the named column label vector was not a valid index")
                        break
                    } else if (as.numeric(unname(custom_column_labels[name])) > nrow(mapping)) {
                        warning("Named column label vector tries to access an index that is out of range")
                        break
                    } else {
                        vector_to_fill[unname(custom_column_labels[name])] = name
                    }
                }
                mapping$custom_labels <- vector_to_fill
            }
            p2 <- p2 + geom_text(data=mapping, aes(x=to, y = y, label=custom_labels), size=font.size, family=family, inherit.aes = FALSE,
                                 angle=colnames_angle, nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
        } else {
            p2 <- p2 + geom_text(data=mapping, aes(x=to, y = y, label=cus), size=font.size, family=family, inherit.aes = FALSE,
                                 angle=colnames_angle, nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
        }
    }

    p2 <- p2 + theme(legend.position="right")
    ## p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))

    if (!colnames) {
        ## https://github.com/GuangchuangYu/ggtree/issues/204
        p2 <- p2 + scale_y_continuous(expand = c(0,0))
    }

    attr(p2, "mapping") <- mapping
    return(p2)
}

