
##' hilight clade with rectangle
##'
##' 
##' @title hilight
##' @param tree_view tree view 
##' @param node clade node
##' @param fill fill color
##' @param alpha alpha
##' @param ... additional parameter
##' @return tree view
##' @export
##' @author Guangchuang Yu
hilight <- function(tree_view, node, fill="steelblue", alpha=0.5, ...) {
    tree_view + geom_hilight(node, fill, alpha, ...)
    ## df <- tree_view$data
    ## sp <- get.offspring.df(df, node)
    ## sp.df <- df[c(sp, node),]
    ## x <- sp.df$x
    ## y <- sp.df$y
    ## tree_view + annotate("rect", xmin=min(x)-df[node, "branch.length"]/2,
    ##                      xmax=max(x), ymin=min(y)-0.5, ymax=max(y)+0.5,
    ##                      fill = fill, alpha = alpha, ...)
}

##' layer of hilight clade with rectangle
##'
##' 
##' @title geom_hilight
##' @param node selected node to hilight
##' @param fill color fill
##' @param alpha alpha (transparency)
##' @param mapping aes mapping
##' @param data data
##' @param stat stat layer
##' @param position position
##' @param show.legend logical
##' @param na.rm logical
##' @param inherit.aes logical
##' @param ... additional parameter
##' @return ggplot2
##' @export
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 GeomRect
##' @author Guangchuang Yu
geom_hilight <- function(node, fill="steelblue", alpha=.5,
                         mapping=NULL, data=NULL, stat="hilight",
                         position="identity", show.legend=NA,
                         na.rm=FALSE, inherit.aes=FALSE, ...) {
    default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    layer(
        stat=StatHilight,
        data = data,
        mapping = mapping,
        geom = GeomRect,
        position = position,
        show.legend=show.legend,
        inherit.aes = inherit.aes,
        params = list(node=node,
            fill=fill, alpha=alpha,
            na.rm = na.rm,
            ...)
        )
    
}

##' stat_hilight
##' @rdname geom_hilight
##' @param geom geometric object
##' @importFrom ggplot2 layer
##' @export
stat_hilight <- function(mapping=NULL, data=NULL, geom="rect",
                         position="identity",  node, 
                         show.legend=NA, inherit.aes=FALSE,
                        fill, alpha,
                         ...) {
    default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }
    
    layer(
        stat=StatHilight,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend=show.legend,
        inherit.aes = inherit.aes,
        params = list(node=node,
            fill=fill, alpha=alpha,
            ...)
        )
}

##' StatHilight
##' @rdname ggtree-ggproto
##' @format NULL
##' @usage NULL
##' @importFrom ggplot2 Stat
##' @export
StatHilight <- ggproto("StatHilight", Stat,
                       compute_group = function(self, data, scales, params, node) {
                           get_clade_position_(data, node)
                       },
                       required_aes = c("x", "y", "branch.length")
                       )


##' get position of clade (xmin, xmax, ymin, ymax)
##'
##' 
##' @title get_clade_position
##' @param treeview tree view
##' @param node selected node
##' @return data.frame
##' @export
##' @author Guangchuang Yu
get_clade_position <- function(treeview, node) {
    get_clade_position_(treeview$data, node)
}

get_clade_position_ <- function(data, node) {
    sp <- get.offspring.df(data, node)
    sp.df <- data[c(sp, node),]
    x <- sp.df$x
    y <- sp.df$y
    data.frame(xmin=min(x)-data[node, "branch.length"]/2,
               xmax=max(x),
               ymin=min(y)-0.5,
               ymax=max(y)+0.5)
}
