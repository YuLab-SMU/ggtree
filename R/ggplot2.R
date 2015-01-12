##' creates a lists of unevaluated expressions
##' @export
##' @rdname aes
##' @param x name values
##' @param y name values
##' @param ... additional name values
##' @source
##' This is just the imported function
##' from the ggplot2 package. The documentation you should
##' read for the aes function can be found here: \link[ggplot2]{aes}
##'
##' @seealso
##' \link[ggplot2]{aes}
aes <- ggplot2::aes


##' generate a ggplot2 plot grob
##' @export
##' @rdname ggplotGrob
##' @param x ggplot2 object
##' @source
##' This is just the imported function
##' from the ggplot2 package. The documentation you should
##' read for the ggplotGrob function can be found here: \link[ggplot2]{ggplotGrob}
##'
##' @seealso
##' \link[ggplot2]{ggplotGrob}
ggplotGrob <- ggplot2::ggplotGrob

##' text annotations
##' @export
##' @rdname geom_text
##' @param mapping the aesthetic mapping
##' @param data A layer specific dataset -
##'             only needed if you want to override he plot defaults.
##' @param stat The statistical transformation to use on the data for this layer
##' @param position The position adjustment to use for overlapping points on this layer
##' @param parse if TRUE, the labels will be passd into expressions
##' @param ... other arguments passed on to 'layer'
##' @source
##' This is just the imported function
##' from the ggplot2 package. The documentation you should
##' read for the geom_text function can be found here: \link[ggplot2]{geom_text}
##'
##' @seealso
##' \link[ggplot2]{geom_text}
geom_text <- ggplot2::geom_text
