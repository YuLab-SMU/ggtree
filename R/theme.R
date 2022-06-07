##' tree theme
##'
##' 'theme_tree' defines a blank background to display tree
##'
##' @title theme_tree
##' @param bgcolor set background color, defaults to "white"
##' @param ... additional parameter
##' @importFrom ggplot2 theme_void
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @export
##' @return updated ggplot object with new theme
##' @author Guangchuang Yu
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree()
theme_tree <- function(bgcolor="white", ...) {

    list(xlab(NULL),
         ylab(NULL),
         theme_tree2_internal() +
         theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
               axis.line.x = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank(),
               ...)
         )
    
    ## theme_void() +
    ##     theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
    ##           ...)
}

##' dendrogram theme
##'
##'
##' @title theme_dendrogram
##' @inheritParams theme_tree
##' @param fgcolor set color of axis
##' @export
##' @importFrom ggplot2 element_text
##' @author Guangchuang Yu
theme_dendrogram <- function(bgcolor = "white", fgcolor = "black", ...) {
    theme_tree2(bgcolor = bgcolor,
                axis.line.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color=fgcolor),
                axis.text.y = element_text(color=fgcolor),
                axis.ticks.y = element_line(color=fgcolor),
                ...)
}


##' tree2 theme
##'
##' 'theme_tree2' supports displaying phylogenetic distance by setting x-axis
##'
##' @title theme_tree2
##' @param bgcolor set background color, defaults to "white"
##' @param fgcolor set foreground color, defaults to "black"
##' @param ... additional parameter
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_line
##' @importFrom ggplot2 element_rect
##' @export
##' @return updated ggplot object with new theme
##' @author Guangchuang Yu
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree2()
theme_tree2 <- function(bgcolor="white", fgcolor="black", ...) {
    list(xlab(NULL),
         ylab(NULL),
         theme_tree2_internal(bgcolor, fgcolor, ...)
         )
}

##' @importFrom ggplot2 theme_bw
theme_tree2_internal <- function(bgcolor="white", fgcolor="black",
                                 legend.position="right",
                                 panel.grid.minor=element_blank(),
                                 panel.grid.major=element_blank(),
                                 panel.border=element_blank(),
                                 axis.line.y=element_blank(),
                                 axis.ticks.y=element_blank(),
                                 axis.text.y=element_blank(),...) {
    ## need to set axis.line otherwise the setting cannot be inherited.
    ## https://github.com/GuangchuangYu/ggtree/issues/218

    theme_bw() +
        theme(legend.position=legend.position,
              panel.grid.minor=panel.grid.minor,
              panel.grid.major=panel.grid.major,
              panel.background=element_rect(fill=bgcolor, colour=bgcolor),
              panel.border=panel.border,
              axis.line=element_line(color=fgcolor),
              ##axis.line.x=element_line(color=fgcolor),
              axis.line.y=axis.line.y,
              axis.ticks.y=axis.ticks.y,
              axis.text.y=axis.text.y,
              ...)
}


##' inset theme
##'
##' theme for inset function
##' @title theme_inset
##' @param legend.position set the position of legend
##' @param ... additional parameter
##' @return ggplot object
##' @export
##' @author Guangchuang Yu
theme_inset <- function(legend.position =  "none", ...) {
    list(xlab(NULL),
         ylab(NULL),
         theme_tree(legend.position = legend.position, plot.margin = unit(c(0, 0, 0, 0), "lines"), ...),
         ggfun::theme_transparent()
         )
}
