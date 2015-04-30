##' view tree and associated matrix
##'
##' @title gplot
##' @param p tree view
##' @param data matrix
##' @param low low color
##' @param high high color
##' @param widths widths of sub plot
##' @param font.size font size
##' @return list of figure
##' @importFrom gridExtra grid.arrange
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_y_continuous
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' nwk <- system.file("extdata", "sample.nwk", package="ggtree")
##' tree <- read.tree(nwk)
##' p <- ggtree(tree)
##' d <- matrix(abs(rnorm(52)), ncol=4)
##' rownames(d) <- tree$tip.label
##' colnames(d) <- paste0("G", 1:4)
##' gplot(p, d, low="green", high="red")
gplot <- function(p, data, low="green", high="red", widths=c(0.5, 0.5), font.size=14) {
    ## p <- p + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0.6))
    p1 <- p + scale_y_continuous(expand = c(0, 0.6))
    ## p1 <- p + theme(panel.margin=unit(0, "null"))
    ## p1 <- p1 + theme(plot.margin = unit(c(1, -1, 1.5, 1), "lines"))
    p2 <- gplot.heatmap(p, data, low, high, font.size)
    grid.arrange(p1, p2, ncol=2, widths=widths)
    invisible(list(p1=p1, p2=p2))
}


##' @importFrom grid unit
##' @importFrom ggplot2 scale_fill_gradient
##' @importFrom ggplot2 scale_fill_discrete
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 guide_legend
##' @importFrom reshape2 melt
gplot.heatmap <- function(p, data, low, high, font.size) {
    isTip <- x <- Var1 <- Var2 <- value <- NULL
    dd=melt(as.matrix(data))
    ## p <- ggtree(tree) ## + theme_tree2()
    ## p <- p + geom_text(aes(x = max(x)*1.1, label=label), subset=.(isTip), hjust=0)
    ## p <- p+geom_segment(aes(x=x*1.02, xend=max(x)*1.08, yend=y), subset=.(isTip), linetype="dashed", size=0.4)
    df=p$data
    df=df[df$isTip,]
    
    dd$Var1 <- factor(dd$Var1, levels = df$label[order(df$y)])
    if (any(dd$value == "")) {
        dd$value[dd$value == ""] <- NA
    }
    
    p2 <- ggplot(dd, aes(Var2, Var1, fill=value))+geom_tile(color="black")
    if (is(dd$value,"numeric")) {
        p2 <- p2 + scale_fill_gradient(low=low, high=high, na.value="white")
    } else {
        p2 <- p2 + scale_fill_discrete(na.value="white")
    }
    
    p2 <- p2+xlab("")+ylab("")
    p2 <- p2+theme_tree2() + theme(axis.ticks.x = element_blank(),
                                   axis.line.x=element_blank())
    ## p1 <- p1 + theme(axis.text.x = element_text(size = font.size))
    p2 <- p2 + theme(axis.ticks.margin = unit(0, "lines")) 
    p2 <- p2 + theme(axis.text.x = element_text(size = font.size))
    ## p2 <- p2 + theme(axis.text.y = element_text(size=font.size))
    
    ## plot.margin   margin around entire plot (unit with the sizes of the top, right, bottom, and left margins) 
    ## units can be given in "lines" or  something more specific like "cm"...

    
    p2 <- p2 + theme(panel.margin=unit(0, "null"))
    p2 <- p2 + theme(plot.margin = unit(c(1, 1, .5, -0.5), "lines"))
    p2 <- p2 + theme(legend.position = "right")
    p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))
    ## p2 <- p2 + labs(fill="")
    
    return(p2)
}


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







