##' layer of hilight clade with rectangle
##'
##' @title geom_hilight
##' @param node selected node to hilight (required)
##' @param fill color fill (default = steelblue)
##' @param alpha alpha transparency, (default = 0.5)
##' @param extend extend xmax of the rectangle (default = 0)
## @param extendto extend xmax to extendto (default = NULL), only works for rectangular and circular/fan layouts
##' @param ... additional parameters
##' @return ggplot2
##' @export
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 GeomRect
##' @author Guangchuang Yu
geom_hilight <- function(node, fill="steelblue", alpha=.5, extend=0, ...) {
    structure(list(node = node,
                   fill = fill,
                   alpha = alpha,
                   extend = extend,
                   ## extendto = extendto,
                   ...),
              class = 'hilight')
}



geom_hilight_rectangular <- function(node, fill="steelblue", alpha=.5, extend=0, extendto=NULL) {
  data = NULL
  stat = "hilight"
  position = "identity"
  show.legend = NA
  na.rm = TRUE
  inherit.aes = FALSE
  check.aes = FALSE

  default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length)
  mapping <- default_aes

  layer(
    stat=StatHilight,
    data = data,
    mapping = mapping,
    geom = GeomRect,
    position = position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    check.aes = check.aes,
    params = list(node=node,
                  fill=fill,
                  alpha=alpha,
                  extend=extend,
                  extendto=extendto,
                  na.rm = na.rm)
  )
}

##' stat_hilight
##'
##' @title stat_hilight
##' @param mapping aes mapping
##' @param data data
##' @param geom geometric object
##' @param position position
##' @param node node number
##' @param show.legend show legend
##' @param inherit.aes logical
##' @param fill fill color
##' @param alpha transparency
##' @param extend extend xmax of the rectangle
##' @param extendto extend xmax to extendto
##' @param ... additional parameter
##' @return layer
##' @importFrom ggplot2 layer
##' @export
stat_hilight <- function(mapping=NULL, data=NULL, geom="rect",
                         position="identity",  node,
                         show.legend=NA, inherit.aes=FALSE,
                         fill, alpha, extend=0, extendto=NULL,
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
                  fill=fill,
                  alpha=alpha,
                  extend=extend,
                  extendto=extendto,
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
                       compute_group = function(self, data, scales, params, node, extend, extendto) {
                           df <- get_clade_position_(data, node)
                           df$xmax <- df$xmax + extend
                           if (!is.null(extendto) && !is.na(extendto)) {
                               if (extendto < df$xmax) {
                                   warning("extendto is too small, keep the original xmax value...")
                               } else {
                                   df$xmax <- extendto
                               }
                           }
                           return(df)
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
    sp <- tryCatch(tidytree:::offspring.tbl_tree(data, node)$node, error=function(e) NULL)
    i <- match(node, data$node)
    if (is.null(sp)) {
        ## tip
        sp.df <- data[i,]
    } else {
        sp <- c(sp, node)
        sp.df <- data[match(sp, data$node),]
    }
    
    x <- sp.df$x
    y <- sp.df$y
    
    if ("branch.length" %in% colnames(data)) {
        xmin <- min(x, na.rm=TRUE)-data[["branch.length"]][i]/2
    } else {
        xmin <- min(sp.df$branch, na.rm=TRUE)
    }
    data.frame(xmin=xmin,
               xmax=max(x, na.rm=TRUE),
               ymin=min(y, na.rm=TRUE) - 0.5,
               ymax=max(y, na.rm=TRUE) + 0.5)
}
