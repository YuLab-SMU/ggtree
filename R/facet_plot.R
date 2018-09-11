##' plot tree associated data in an additional panel
##'
##'
##' 'facet_plot()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' and other parameters,
##' and align the graph with the tree 'p' side by side.
##' @title facet_plot
##' @param p tree view
##' @param panel panel name for plot of input data
##' @param data data to plot by 'geom', first column should be matched with tip label of tree
##' @param geom geom function to plot the data
##' @param mapping aes mapping for 'geom'
##' @param ... additional parameters for 'geom'
##' @return ggplot object
##' @examples
##' tr <- rtree(10)
##' dd = data.frame(id=tr$tip.label, value=abs(rnorm(10)))
##' p <- ggtree(tr)
##' facet_plot(p, 'Trait', data = dd, geom=geom_point, mapping=aes(x=value))
##' @export
##' @author Guangchuang Yu
facet_plot <- function(p, panel, data, geom, mapping=NULL, ...) {
    p <- add_panel(p, panel)
    df <- p %+>% data
    p + geom(data=df, mapping=mapping, ...)
}

##' label facet_plot output
##'
##' 
##' @title facet_labeller
##' @param p facet_plot output
##' @param label labels of facet panels
##' @return ggplot object
##' @importFrom ggplot2 labeller
##' @export
##' @author Guangchuang Yu
facet_labeller <- function(p, label) {
    lbs  <-  levels(p$data$.panel)
    names(lbs)  <-  lbs
    label <- label[names(label) %in% lbs]
    lbs[names(label)]  <-  label

    p + facet_grid( . ~ .panel, scales="free_x",
               labeller = labeller(.panel = lbs))
}

##' @importFrom ggplot2 facet_grid
add_panel <- function(p, panel) {
    df <- p$data
    if (is.null(df[[".panel"]])) {
        df[[".panel"]] <- factor("Tree")
    }
    levels(df$.panel) %<>% c(., panel)
    p$data <- df
    p + facet_grid(.~.panel, scales="free_x")
}

