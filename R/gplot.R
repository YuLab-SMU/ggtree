##' view tree and associated matrix
##' 
##' @param tree one of phylo, phylo4 or jplace object
##' @param data matrix
##' @param low low color
##' @param high high color
##' @param widths widths of sub plot
##' @param font.size font size
##' @importFrom reshape2 melt
##' @importFrom gridExtra grid.arrange
##' @importFrom grid unit
##' @importFrom ggplot2 scale_fill_gradient
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 geom_tile
##' @export
gplot <- function(tree, data, low="green", high="red", widths=c(0.5, 0.5), font.size=14) {
    isTip <- x <- Var1 <- Var2 <- value <- NULL
    dd=melt(as.matrix(data))
    p <- ggtree(tree) ## + theme_tree2()
    ## p <- p + geom_text(aes(x = max(x)*1.1, label=label), subset=.(isTip), hjust=0)
    ## p <- p+geom_segment(aes(x=x*1.02, xend=max(x)*1.08, yend=y), subset=.(isTip), linetype="dashed", size=0.4)
    df=p$data
    df=df[df$isTip,]
    
    levels(dd$Var1) <- df$label[order(df$y)]

    p2 <- ggplot(dd, aes(Var2, Var1, fill=value))+geom_tile(color="black")
    p2 <- p2 + scale_fill_gradient(low=low, high=high)
    p2 <- p2+xlab("")+ylab("")
    p2 <- p2+theme_tree2() + theme(axis.ticks.x = element_blank(),
                                   axis.line.x=element_blank())
    ## p1 <- p1 + theme(axis.text.x = element_text(size = font.size))
    p2 <- p2 + theme(axis.ticks.margin = unit(0, "lines")) 
    p2 <- p2 + theme(axis.text.x = element_text(size = font.size))
    p2 <- p2 + theme(axis.text.y = element_text(size=font.size))
    
    ## plot.margin   margin around entire plot (unit with the sizes of the top, right, bottom, and left margins) 
    ## units can be given in "lines" or  something more specific like "cm"...

    p1 <- p + theme(panel.margin=unit(0, "null"))
    p2 <- p2 + theme(panel.margin=unit(0, "null"))
    p1 <- p1 + theme(plot.margin = unit(c(1, -0.5, 1.5, 1), "lines"))
    p2 <- p2 + theme(plot.margin = unit(c(1, 1, 1, -0.5), "lines"))
    p2 <- p2 + theme(legend.position = "right")
    grid.arrange(p1, p2, ncol=2, widths=widths)

}
