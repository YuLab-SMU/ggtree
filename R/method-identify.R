
##' identify node by interactive click
##'
##'
##' @rdname identify
##' @title identify
##' @param x tree view
##' @param col selected columns to extract. Default is "auto" which will select all columns for 'ggplot' object and 'node' column for 'ggtree' object
##' @param ... additional parameters, normally ignored
##' @return closest data point
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
identify.gg <- function(x = last_plot(), col = "auto", ...) {
    ## tree_view <- x
    ## x=NULL, it will call graphics::identify

    ## x <- tree_view$data$x
    ## y <- tree_view$data$y

    plot <- x
    xvar <- ggfun::get_aes_var(plot$mapping, 'x')
    yvar <- ggfun::get_aes_var(plot$mapping, 'y')
    x <- plot$data[[xvar]]
    y <- plot$data[[yvar]]

    xlim <- aplot::xrange(plot)
    ylim <- aplot::yrange(plot)
    x <- c(x, rep(xlim, times = 2))
    y <- c(y, rep(ylim, each = 2))

    pushViewport(dataViewport(x, y))
    loc <- grid.locator('in') %>% as.numeric

    xx <- as.numeric(convertX( unit(x,'native'), 'in' ))
    yy <- as.numeric(convertY( unit(y,'native'), 'in' ))

    idx <- which.min( (xx-loc[1])^2 + (yy-loc[2])^2 )
    res <- plot$data[idx,]
    if (col == "auto" && inherits(plot, 'ggtree')) {
        col <- 'node'
    }
    if (length(col) == 1 && col == "auto") {
        return(res)
    }

    res <- res[,col]
    if (length(col) == 1) {
        res <- res[[1]]
    }
    return(res)
}

