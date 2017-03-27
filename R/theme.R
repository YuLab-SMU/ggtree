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
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @export
##' @return updated ggplot object with new theme
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr) + theme_tree()
theme_tree <- function(bgcolor="white", fgcolor="black", ...) {
    list(xlab(NULL),
         ylab(NULL),
	 theme_tree2_internal() +
         theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
               axis.line.x = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank(),
               ...)
	 )
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
##' @importFrom ggplot2 element_rect
##' @export
##' @return updated ggplot object with new theme
##' @author Yu Guangchuang
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

theme_tree2_internal <- function(bgcolor="white", fgcolor="black",
                                 legend.position="none",
                                 panel.grid.minor=element_blank(),
                                 panel.grid.major=element_blank(),
                                 panel.border=element_blank(),
                                 axis.line.y=element_blank(),
                                 axis.ticks.y=element_blank(),
                                 axis.text.y=element_blank(),...) {
    theme_bw() +
        theme(legend.position=legend.position,
              panel.grid.minor=panel.grid.minor,
              panel.grid.major=panel.grid.major,
              panel.background=element_rect(fill=bgcolor, colour=bgcolor),
              panel.border=panel.border,
              ## axis.line=element_line(color=fgcolor),
              axis.line.x=element_line(color=fgcolor),
              axis.line.y=axis.line.y,
              axis.ticks.y=axis.ticks.y,
              axis.text.y=axis.text.y,
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
    message("this theme was moved to ggimage::theme_transparent and will be removed in next release")

    theme(panel.background = element_rect(
              fill = "transparent",
              colour = NA),
          plot.background = element_rect(
              fill = "transparent",
              colour = NA),
          legend.key = element_rect(
              fill = "transparent",
              colour = NA),
          legend.background = element_rect(
              fill = "transparent",
              colour = NA), ...)
}

##' inset theme
##'
##' theme for inset function
##' @title theme_inset
##' @param ... additional parameter
##' @return ggplot object
##' @export
##' @author Guangchuang Yu
theme_inset <- function(...) {
    message("this theme will be removed in next release")
    list(xlab(NULL),
         ylab(NULL),
         theme_tree(...),
         theme_transparent()
         )
}
