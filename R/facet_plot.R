##' plot tree associated data in an additional panel
##'
##'
##' 'facet_plot()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' and other parameters,
##' and align the graph with the tree 'p' side by side. 'geom_facet' is a 'ggplot2' layer version of 'facet_plot'
##' @title facet_plot
##' @rdname facet-plot
##' @param p tree view
##' @param mapping aes mapping for 'geom' 
##' @param data data to plot by 'geom', first column should be matched with tip label of tree
##' @param geom geom function to plot the data
##' @param panel panel name for plot of input data
##' @param ... additional parameters for 'geom'
##' @return ggplot object
##' @examples
##' tr <- rtree(10)
##' dd = data.frame(id=tr$tip.label, value=abs(rnorm(10)))
##' p <- ggtree(tr)
##' facet_plot(p, 'Trait', data = dd, geom=geom_point, mapping=aes(x=value))
##' @export
##' @author Guangchuang Yu
##' @references G Yu, TTY Lam, H Zhu, Y Guan (2018). Two methods for mapping and visualizing associated data
##' on phylogeny using ggtree. Molecular Biology and Evolution, 35(2):3041-3043.
##' <https://doi.org/10.1093/molbev/msy194>
facet_plot <- function(p, mapping=NULL, data, geom, panel, ...) {
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


##' extract data used in `facet_plot` or `geom_facet`
##'
##' 
##' @title facet_data
##' @param tree_view ggtree object
##' @param panel data plotted in specific panel. If only one dataset used in the panel, return the data frame, else return a list of data frames.
##' @return data frame or a list of data frames
##' @export
##' @author Guangchuang Yu
##' @references G Yu, TTY Lam, H Zhu, Y Guan (2018). Two methods for mapping and visualizing associated data
##' on phylogeny using ggtree. Molecular Biology and Evolution, 35(2):3041-3043.
##' <https://doi.org/10.1093/molbev/msy194>
facet_data <- function(tree_view, panel) {
    n <- length(tree_view$layers)
    j <- which(vapply(1:n, function(i) {
        d <- tree_view$layers[[i]]$data
        if(is.null(d$.panel))
            return(FALSE)
        d$.panel[1] == panel
    }, logical(1)))

    d <- tree_view$data
    res <- lapply(j, function(i) {        
        d2 <- tree_view$layers[[i]]$data

        lb <- which(names(d2) == 'label')
        v <- which(!names(d2) %in% names(d))

        d2 <- d2[,c(lb, v)]
        d2[order(rownames(d2)),]
    })
    if (length(j) == 1)
        return(res[[1]])
    return(res)
}

