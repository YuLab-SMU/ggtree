##' geom layer to draw aligned motif
##'
##'
##' @title geom_motif
##' @param mapping aes mapping
##' @param data data
##' @param on gene to center (i.e. set middle position of the `on` gene to 0)
##' @param ... additional parameters
##' @return geom layer
##' @export
##' @author guangchuang yu
geom_motif <- function(mapping, data, on, ...) {
    if (is.null(unlist(mapping)$y)) {
        seqnames <- as.character(unlist(mapping)$group)
    } else {
        seqnames <- as.character(unlist(mapping)$y)
    }

    if (is.null(unlist(mapping$fill))) {
        id <- as.character(unlist(mapping$id))
    } else {
        id <- as.character(unlist(mapping$fill))
    }
    dd <- data[unlist(data[, id]) == on,]
    mid <- dd$start + (dd$end - dd$start)/2
    names(mid) <- as.character(unlist(dd[, seqnames]))
    adj <- mid[as.character(unlist(data[, seqnames]))]
    data$start <- data$start - adj
    data$end <- data$end - adj
    geom_gene_arrow <- get_fun_from_pkg("gggenes", "geom_gene_arrow")
    geom_gene_arrow(mapping = mapping, data = as.data.frame(data), ...)
}


plot_fantree <- function(fantree, upper=TRUE) {
    if (upper) {
        ymin <- -.25
        ymax <- 1.3
        ## y <- 0.55
    } else {
        ymin <- .2
        ymax <- 1.75
        ## y <- 0.45
    }

    ggplot() + xlim(0,1) + ylim(0.5, 1) + theme_tree() +
        annotation_custom(ggplotGrob(fantree),
                          xmin=-.15, xmax=1.15,
                          ymin=ymin, ymax=ymax)
}

plot_fantrees <- function(uppertree, lowertree) {
    ggplot() + xlim(0,1) + ylim(0.5, 1) + theme_tree() +
        annotation_custom(ggplotGrob(uppertree), xmin=-.15, xmax=1.15, ymin=0.52, ymax=1.02) +
        annotation_custom(ggplotGrob(lowertree + ggimage::theme_transparent()), xmin=-.15, xmax=1.15, ymin=0.48, ymax=0.98)
}



##' return a data.frame that contains position information
##' for labeling column names of heatmap produced by `gheatmap` function
##'
##'
##' @title get_heatmap_column_position
##' @param treeview output of `gheatmap`
##' @param by one of 'bottom' or 'top'
##' @return data.frame
##' @export
##' @author Guangchuang Yu
get_heatmap_column_position <- function(treeview, by="bottom") {
    by %<>% match.arg(c("bottom", "top"))

    mapping <- attr(treeview, "mapping")
    if (is.null(mapping)) {
        stop("treeview is not an output of `gheatmap`...")
    }

    colnames(mapping) <- c("label", "x")
    if (by == "bottom") {
        mapping$y <- 0
    } else {
        mapping$y <- max(treeview$data$y) + 1
    }
    return(mapping)
}

##' scale x for tree with heatmap
##'
##'
##' @title scale_x_ggtree
##' @param tree_view tree view
##' @param breaks breaks for tree
##' @param labels lables for corresponding breaks
##' @return tree view
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_x_date
##' @export
##' @author Guangchuang Yu
scale_x_ggtree <- function(tree_view, breaks=NULL, labels=NULL) {
    p <- get_tree_view(tree_view)

    mrsd <- get("mrsd", envir=tree_view$plot_env)
    if (!is.null(mrsd) && class(p$data$x) == "Date") {
        x <- Date2decimal(p$data$x)
    } else {
        x <- p$data$x
    }

    if (is.null(breaks)) {
        breaks <- hist(x, breaks=5, plot=FALSE)$breaks
    }
    m <- attr(p, "mapping")

    if (!is.null(mrsd) &&class(m$to) == "Date") {
        to <- Date2decimal(m$to)
    } else {
        to <- m$to
    }

    idx <- which(sapply(breaks, function(x) any(x > m$to)))
    if (length(idx)) {
        breaks <- breaks[-idx]
    }

    if (is.null(labels)) {
        labels <- breaks
    }

    breaks <- c(breaks, to)
    labels <- c(labels, gsub("\\.", "", as.character(m$from)))

    if (!is.null(mrsd) && class(p$data$x) == "Date") {
        p <- p + scale_x_date(breaks=decimal2Date(breaks), labels)
    } else {
        p <- p + scale_x_continuous(breaks=breaks, labels=labels)
    }
    return(p)
}



## ##' view tree and associated matrix
## ##'
## ##' @title gplot
## ##' @param p tree view
## ##' @param data matrix
## ##' @param low low color
## ##' @param high high color
## ##' @param widths widths of sub plot
## ##' @param color color
## ##' @param font.size font size
## ##' @return list of figure
## ##' @importFrom gridExtra grid.arrange
## ##' @importFrom ggplot2 scale_x_continuous
## ##' @importFrom ggplot2 scale_y_continuous
## ##' @export
## ##' @author Guangchuang Yu \url{http://ygc.name}
## ##' @examples
## ##' nwk <- system.file("extdata", "sample.nwk", package="ggtree")
## ##' tree <- read.tree(nwk)
## ##' p <- ggtree(tree)
## ##' d <- matrix(abs(rnorm(52)), ncol=4)
## ##' rownames(d) <- tree$tip.label
## ##' colnames(d) <- paste0("G", 1:4)
## ##' gplot(p, d, low="green", high="red")
## gplot <- function(p, data, low="green", high="red", widths=c(0.5, 0.5), color="white", font.size=14) {
##     ## p <- p + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0.6))
##     p1 <- p + scale_y_continuous(expand = c(0, 0.6))
##     ## p1 <- p + theme(panel.margin=unit(0, "null"))
##     ## p1 <- p1 + theme(plot.margin = unit(c(1, -1, 1.5, 1), "lines"))
##     p2 <- gplot.heatmap(p, data, low, high, color, font.size)
##     grid.arrange(p1, p2, ncol=2, widths=widths)
##     invisible(list(p1=p1, p2=p2))
## }


## ##' @importFrom grid unit
## ##' @importFrom ggplot2 scale_fill_gradient
## ##' @importFrom ggplot2 scale_fill_discrete
## ##' @importFrom ggplot2 element_text
## ##' @importFrom ggplot2 geom_tile
## ##' @importFrom ggplot2 labs
## ##' @importFrom ggplot2 guides
## ##' @importFrom ggplot2 guide_legend
## ##' @importFrom reshape2 melt
## gplot.heatmap <- function(p, data, low, high, color="white", font.size) {
##     isTip <- x <- Var1 <- Var2 <- value <- NULL
##     dd=melt(as.matrix(data))
##     ## p <- ggtree(tree) ## + theme_tree2()
##     ## p <- p + geom_text(aes(x = max(x)*1.1, label=label), subset=.(isTip), hjust=0)
##     ## p <- p+geom_segment(aes(x=x*1.02, xend=max(x)*1.08, yend=y), subset=.(isTip), linetype="dashed", size=0.4)
##     df=p$data
##     df=df[df$isTip,]

##     dd$Var1 <- factor(dd$Var1, levels = df$label[order(df$y)])
##     if (any(dd$value == "")) {
##         dd$value[dd$value == ""] <- NA
##     }

##     p2 <- ggplot(dd, aes(Var2, Var1, fill=value))+geom_tile(color=color)
##     if (is(dd$value,"numeric")) {
##         p2 <- p2 + scale_fill_gradient(low=low, high=high, na.value="white")
##     } else {
##         p2 <- p2 + scale_fill_discrete(na.value="white")
##     }

##     p2 <- p2+xlab("")+ylab("")
##     p2 <- p2+theme_tree2() + theme(axis.ticks.x = element_blank(),
##                                    axis.line.x=element_blank())
##     ## p1 <- p1 + theme(axis.text.x = element_text(size = font.size))
##     p2 <- p2 + theme(axis.ticks.margin = unit(0, "lines"))
##     p2 <- p2 + theme(axis.text.x = element_text(size = font.size))
##     ## p2 <- p2 + theme(axis.text.y = element_text(size=font.size))

##     ## plot.margin   margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
##     ## units can be given in "lines" or  something more specific like "cm"...


##     p2 <- p2 + theme(panel.margin=unit(0, "null"))
##     p2 <- p2 + theme(plot.margin = unit(c(1, 1, .5, -0.5), "lines"))
##     p2 <- p2 + theme(legend.position = "right")
##     p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))
##     ## p2 <- p2 + labs(fill="")

##     return(p2)
## }


coplot <- function(tree1, tree2, hjust=0) {
    x <- y <- label <- isTip <- tree <- NULL
    dx <- fortify(tree1)
    dx$tree <- "A"

    offset <- max(dx$x) * 1.3
    dy <- fortify(tree2)
    dy <- reverse.treeview.data(dy)
    dy$x <- dy$x + offset + hjust
    dy$tree <- "B"

    dd <- rbind(dx, dy)
    p <- ggplot(dd, aes(x, y)) +
        geom_tree(layout="phylogram", subset=.(tree=="A")) +
            geom_tree(layout="phylogram", subset=.(tree=="B")) +
                theme_tree()

    p <- p  + geom_text(aes(label=label),
                        subset=.(isTip & tree == "A"),
                        hjust=-offset/40) +
                            geom_text(aes(label=label),
                                      subset=.(isTip & tree == "B"),
                                      hjust = offset/20)
    return(p)
}





##' set legend for multiple geom_hilight layers
##'
##'
##' @title set_hilight_legend
##' @param p ggtree object
##' @param color color vector
##' @param label label vector
##' @param alpha transparency of color
##' @return updated ggtree object
##' @export
##' @importFrom scales alpha
##' @author guangchuang yu
set_hilight_legend <- function(p, color, label, alpha=1) {
	d <- data.frame(color=color, clade=label, x=0, y=1, alpha=alpha)
	p + geom_rect(aes_(fill=~clade, xmin=~x, xmax=~x, ymin=~y, ymax=~y), data=d, inherit.aes=F) +
		guides(fill=guide_legend(override.aes=list(fill=alpha(d$color, d$alpha))))
}
