##' plot tree associated data in an additional panel
##'
##'
##' 'facet_plot()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' and other parameters,
##' and align the graph with the tree 'p' side by side. 'geom_facet' is a 'ggplot2' layer version of 'facet_plot'
##' @title facet_plot
##' @rdname facet-plot
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
    p + geom_facet(panel = panel, data = data,
                   geom = geom, mapping = mapping, ...)
}

##' @rdname facet-plot
##' @export
geom_facet <- function(mapping=NULL, data, geom, panel, ...) {
    params <- list(...)
    structure(list(panel = panel, data = data,
                   geom = geom, mapping = mapping,
                   params = params), class = 'facet_plot')
}



