##' plot multiple ggplot objects in one page
##'
##' 
##' @title multiplot
##' @param ... plots
##' @param plotlist plot list
##' @param ncol number of column
##' @param widths widths of plots 
##' @param labels labels for labeling the plots
##' @param label_size font size of label
##' @return plot
##' @importFrom grid grid.newpage
##' @importFrom grid unit
##' @importFrom grid viewport
##' @importFrom grid pushViewport
##' @importFrom grid grid.layout
##' @export
##' @author Guangchuang Yu
multiplot <- function(..., plotlist=NULL, ncol, widths = rep_len(1, ncol), labels=NULL, label_size=5) {
    plots <- c(list(...), plotlist)
    
    n <- length(plots)
    layout <- matrix(seq(1, ncol * ceiling(n/ncol)),
                     ncol = ncol, nrow = ceiling(n/ncol))

    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), widths=unit(widths, "null"))))
    for (i in 1:n) {
        ii <- as.data.frame(which(layout == i, arr.ind = TRUE))
        p <- plots[[i]]
        
        if (!is.null(labels)) {
            x <- p$data$x %>% min
            y <- p$data$y %>% max
            p <- p + annotate("text", x=x, y=y, label=labels[i], size=label_size, fontface='bold', hjust=-.5, vjust=-.5)
        }
        print(p, vp = viewport(layout.pos.row = ii$row,
                               layout.pos.col = ii$col)
              )
    }
}

