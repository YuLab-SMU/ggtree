##' append a heatmap of a matrix to the right side of a phylogenetic tree
##'
##'
##' @title gheatmap
##' @param p tree view
##' @param data matrix or data.frame
##' @param offset set offset of the heatmap to tree
##' @param width total width of heatmap, compare to width of tree, defaults to 1,
##' which means they are of the same length
##' @param low set color of the lowest value, defaults to "green"
##' @param high set color of the highest value, defaults to "red"
##' @param color set color of heatmap cell border, defaults to "white"
##' @param colnames logical, whether to add matrix colnames, defaults to "TRUE"
##' @param colnames_position set the position of the colnames, one of 'bottom' (default) or 'top'
##' @param colnames_angle set the angle of colnames
##' @param colnames_level set levels of colnames
##' @param colnames_offset_x set x offset for colnames
##' @param colnames_offset_y set y offset for colnames
##' @param font.size set font size of matrix colnames
##' @param family font of matrix colnames, can be any supported font
##' @param hjust adjust horizonal position of column names (0: align left, 0.5: align center (default), 1: align righ)
##' @param legend_title title of fill legend
##' @param custom_column_labels instead of using the colnames from the input matrix/data.frame, 
##' input a custom vector to be set as column labels
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
##' @references
##' For demonstration of this function, please refer to chapter 7.3 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
gheatmap <- function(p, data, offset=0, width=1, low="green", high="red", color="white",
                     colnames=TRUE, colnames_position="bottom", colnames_angle=0, colnames_level=NULL,
                     colnames_offset_x = 0, colnames_offset_y = 0, font.size=4, family="",
                     hjust=0.5, legend_title = "value", custom_column_labels = NULL) {

    colnames_position %<>% match.arg(c("bottom", "top"))
    variable <- value <- lab <- y <- NULL

    ## if (is.null(width)) {
    ##     width <- (p$data$x %>% range %>% diff)/30
    ## }

    ## convert width to width of each cell
    width <- width * (p$data$x %>% range(na.rm=TRUE) %>% diff) / ncol(data)

    isTip <- x <- from <- to <- custom_labels <- NULL

    ## handle the display of heatmap on collapsed nodes
    ## https://github.com/GuangchuangYu/ggtree/issues/242
    ## extract data on leaves (& on collapsed internal nodes) 
    ## (the latter is extracted only when the input data has data on collapsed
    ## internal nodes)
    df <- p$data
    nodeCo <- intersect(df %>% dplyr::filter(is.na(x)) %>% 
                         select(.data$parent, .data$node) %>% unlist(), 
                     df %>% dplyr::filter(!is.na(x)) %>% 
                         select(.data$parent, .data$node) %>% unlist())
    labCo <- df %>% dplyr::filter(.data$node %in% nodeCo) %>% 
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
    data_axis <- data.frame(from=dd$variable, to=V2)
    data_axis <- unique(data_axis)

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
        data_axis$y <- y
        data_axis[[".panel"]] <- factor("Tree")
        # if custom column annotations are provided
        if (!is.null(custom_column_labels)) {
            # assess the type of input for the custom column annotation
            # either a vector or a named vector with positions for specific names
            if (is.null(names(custom_column_labels))) {
                if (length(custom_column_labels) > nrow(data_axis)) {
                    warning(paste("Input column label vector has more elements than there are columns.",
                                  "\n", "Using the first ", nrow(data_axis)," elements as labels", sep=""))
                    data_axis[["custom_labels"]] <- as.character(custom_column_labels[1:nrow(data_axis)])
                 } else if (length(custom_column_labels) < nrow(data_axis)) {
                        warning(paste("Input column label vector has fewer elements than there are columns.",
                                   "\n", "Using all available labels, n = ",
                                   length(custom_column_labels), sep=""))
                        data_axis[["custom_labels"]] <- as.character(c(custom_column_labels,
                                rep("", nrow(data_axis) - length(custom_column_labels))))
                 } else {
                     data_axis[["custom_labels"]] <- custom_column_labels
                    }
            } else {
                if (!is.null(colnames_level)) {
                    # use the colnames levels if available
                    # otherwise use the default order provided by the data frame
                    vector_order <- colnames_level
                    
                } else {
                    vector_order <- as.character(data_axis$from)
                }
                for (elem in custom_column_labels) {
                    vector_order[which(vector_order == elem)] = names(which(custom_column_labels == elem))
                }
                data_axis[["custom_labels"]] <- vector_order
                }
            p2 <- p2 + geom_text(data=data_axis, aes(x=to, y = y, label=custom_labels),
                                 size=font.size, family=family, inherit.aes = FALSE, angle=colnames_angle,
                                 nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
        } else {
            p2 <- p2 + geom_text(data=data_axis, aes(x=to, y = y, label=from), size=font.size, family=family,
                                 inherit.aes = FALSE, angle=colnames_angle,
                                 nudge_x=colnames_offset_x, nudge_y = colnames_offset_y, hjust=hjust)
        }
    }
    p2 <- p2 + theme(legend.position="right")
    ## p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))

    if (!colnames) {
        ## https://github.com/GuangchuangYu/ggtree/issues/204
        p2 <- p2 + scale_y_continuous(expand = c(0,0))
    }

    attr(p2, "data_axis") <- data_axis
    return(p2)
}

