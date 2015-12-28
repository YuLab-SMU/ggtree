##' tree theme
##'
##' 
##' @title theme_tree
##' @param bgcolor background color
##' @param fgcolor foreground color
##' @param ... additional parameter
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 %+replace%
##' @export
##' @return updated ggplot object with new theme
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree()
theme_tree <- function(bgcolor="white", fgcolor="black", ...) {
    theme_tree2() %+replace%
    theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          ...)
}

##' tree2 theme
##'
##' 
##' @title theme_tree2
##' @param bgcolor background color
##' @param fgcolor foreground color
##' @param ... additional parameter
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_line
##' @importFrom ggplot2 %+replace%
##' @importFrom ggplot2 element_rect
##' @export
##' @return updated ggplot object with new theme
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree2()
theme_tree2 <- function(bgcolor="white", fgcolor="black", ...) {
    theme_bw() %+replace%
    theme(legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background=element_rect(fill=bgcolor, colour=bgcolor),
          panel.border=element_blank(),
          axis.line=element_line(color=fgcolor),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          ...)
}

##' transparent background theme
##'
##' 
##' @title theme_transparent
##' @param ... additional parameter to tweak the theme
##' @return ggplot object
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_rect
##' @export
##' @author Guangchuang Yu
theme_transparent <- function(...) {
    theme(panel.background = element_rect(
              fill = "transparent",
              colour = NA),
          plot.background = element_rect(
              fill = "transparent",
              colour = NA), ...)
}
