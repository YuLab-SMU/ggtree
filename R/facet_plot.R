##' plot tree associated data in an additional panel
##'
##'
##' @title facet_plot
##' @param p tree view
##' @param panel panel name for plot of input data
##' @param data data to plot by 'geom', first column should be matched with tip label of tree
##' @param geom geom function to plot the data
##' @param mapping aes mapping for 'geom'
##' @param ... additional parameters for 'geom'
##' @return ggplot object
##' @export
##' @author Guangchuang Yu
facet_plot <- function(p, panel, data, geom, mapping=NULL, ...) {
    p <- add_panel(p, panel)
    df <- p %+>% data
    p + geom(data=df, mapping=mapping, ...)
}

##' @importFrom ggplot2 facet_grid
add_panel <- function(p, panel) {
    df <- p$data
    if (is.null(df$panel)) {
        df$panel <- factor("Tree")
    }
    levels(df$panel) %<>% c(., panel)
    p$data <- df
    p + facet_grid(.~panel, scales="free_x")
}

##' set x axis limits for Tree panel
##'
##'
##' @title set_tree_xlim
##' @param tree_view tree view
##' @param xlim xlim, should be of length 2
##' @return updated tree view
##' @export
##' @importFrom ggplot2 geom_blank
##' @author guangchuang yu
set_tree_xlim <- function(tree_view, xlim) {
    if (length(xlim) != 2) {
        stop('-> xlim should be of length 2...')
    }
    x <- tree_view$data$x
    if (is.na(xlim[1])) {
        xlim[1] <- min(x)
    }
    if (is.na(xlim[2])) {
        xlim[2] <- max(x)
    }
    dummy <- data.frame(x=xlim, panel='Tree')
    tree_view + geom_blank(aes(x=x), dummy, inherit.aes = FALSE)
}

