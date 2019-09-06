
##' identify node by interactive click
##'
##'
##' @rdname identify
##' @title identify
##' @param x tree view
##' @param ... additional parameters
##' @return node id
##' @importFrom grid convertX
##' @importFrom grid convertY
##' @importFrom grid pushViewport
##' @importFrom grid grid.locator
##' @importFrom grid unit
##' @importFrom grid dataViewport
##' @importFrom graphics identify
##' @importFrom ggplot2 last_plot
##' @method identify gg
##' @export
##' @author Guangchuang Yu
identify.gg <- function(x = last_plot(), ...) {
    tree_view <- x
    ## x=NULL, it will call graphics::identify

    x <- tree_view$data$x
    y <- tree_view$data$y

    pushViewport(dataViewport(x, y))
    loc <- grid.locator('in') %>% as.numeric

    xx <- as.numeric(convertX( unit(x,'native'), 'in' ))
    yy <- as.numeric(convertY( unit(y,'native'), 'in' ))

    idx <- which.min( (xx-loc[1])^2 + (yy-loc[2])^2 )
    return(tree_view$data$node[idx])
}

