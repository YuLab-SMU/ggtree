
##' add tip point
##'
##'
##' @title geom_tippoint
##' @inheritParams geom_point2
##' @return tip point layer
##' @export
##' @author Guangchuang Yu
geom_tippoint <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    self_mapping <- aes_(node = ~node, subset = ~isTip)
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        if (is.null(mapping$subset)) {
            mapping <- modifyList(self_mapping, mapping)   
        } else { 
            mapping <- modifyList(self_mapping, mapping)
            subset_mapping <- aes_string(subset = paste0(
                                             as.expression(get_aes_var(mapping, "subset")),
                                             '&isTip')
                                         )
            mapping <- modifyList(mapping, subset_mapping)
        }
    }
    geom_point2(mapping, data, position, na.rm, show.legend, inherit.aes, stat = StatTreeData, ...)
}

## angle is not supported,
## https://github.com/GuangchuangYu/ggtree/issues/77
##
##
## geom_tippoint2 <- function(mapping=NULL, hjust=0, ...) {
##     angle <- NULL
##     isTip <- NULL
##     m1 <- aes(subset=(isTip & (angle < 90 | angle > 270)), angle=angle)
##     m2 <- aes(subset=(isTip & (angle >= 90 & angle <=270)), angle=angle+180)

##     if (!is.null(mapping)) {
##         m1 <- modifyList(mapping, m1)
##         m2 <- modifyList(mapping, m2)
##     }

##     list(geom_tippoint(m1, hjust=hjust, ...),
##          geom_tippoint(m2, hjust=1-hjust, ...)
##          )
## }


##' add node point
##'
##'
##' @title geom_nodepoint
##' @inheritParams geom_point2
##' @return node point layer
##' @importFrom ggplot2 aes_string
##' @export
##' @author Guangchuang Yu
geom_nodepoint <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
    self_mapping <- aes_(node = ~node, subset = ~ (!isTip))
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        if (is.null(mapping$subset)) {
            mapping <- modifyList(self_mapping, mapping)   
        } else {
            mapping <- modifyList(self_mapping, mapping)
            subset_mapping <- aes_string(subset = paste0(
                                             as.expression(get_aes_var(mapping, "subset")),
                                             '&!isTip')
                                         )
            mapping <- modifyList(mapping, subset_mapping)               
        }
    }
    geom_point2(mapping, data, position, na.rm, show.legend, inherit.aes, stat = StatTreeData, ...)
}


##' geom_rootpoint is used to add root point
##'
##' geom_rootpoint inherit from geom_point2, it is used to display and customize the points on the root
##'
##' @title geom_rootpoint
##' @inheritParams geom_point2
##' @return root point layer
##' @export
##' @author Guangchuang Yu
##' @references `r ggtree:::ggtree_references()`
##' @seealso
##'  [geom_point][ggplot2::geom_point]; 
##'  [geom_rootpoint] add point of root; 
##'  [geom_nodepoint] add points of internal nodes; 
##'  [geom_tippoint] add points of external nodes (also known as tips or leaves).
##' @examples
##' library(ggtree)
##' tr <- rtree(10)
##' ##  add root point
##' ggtree(tr) + geom_rootpoint()
##' ggtree(tr) + geom_rootpoint(size=2,color="red",shape=2)
geom_rootpoint <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
    isTip <- node <- parent <- NULL
    self_mapping <- aes(node = node, subset = (node == parent))
    if (is.null(mapping)) {
        mapping <- self_mapping
    } else {
        if (is.null(mapping$subset)) {
            mapping <- modifyList(self_mapping, mapping)               
        } else {
            mapping <- modifyList(self_mapping, mapping)
            subset_mapping <- aes_string(subset = paste0(
                                             as.expression(get_aes_var(mapping, "subset")),
                                             '&node==parent')
                                         )
            mapping <- modifyList(mapping, subset_mapping)   
        }


    }
    geom_point2(mapping, data, position, na.rm, show.legend, inherit.aes, stat = StatTreeData, ...)
}


#' geom_point2 is a modified version of geom_point that supports aes(subset)
#'
#' `geom_point2` creates scatterplots, just similar to `ggplot2::geom_point`. It extends the `ggplot2::geom_point` to support filtering via the `subset` aesthetic mapping (see Aesthetics section).
#'
#'
#' @title geom_point2
#' @param mapping Set of aesthetic mapping created by `aes()`.
#' If `inherit.aes = TRUE`, the mapping can be inherited from the plot mapping as
#' specified in the call to `ggplot()`.
#' @param data The data to be displayed in this layer. If 'NULL' (the default),
#' the data is inherited from the plot data as specified in the call to 'ggplot()',
#' @param stat Name of the statistical transformation to be used on the data for this layer.
#' @param position Position adjustment.
#' @param na.rm logical. If 'FALSE' (the default), missing values are removed with a warning. If 'TRUE', missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' 'NA', the default, includes if any aesthetics are mapped. 'FALSE' never includes, and 'TRUE' always includes.
#' @param inherit.aes logical (default is 'TRUE'). If 'FALSE', overrides the default aesthetics,
#' rather then combining with them.
#' @param ... addtional parameters that passed on to this layer. These are often aesthetics, used to set an aesthetic to a fixed value, like `colour = "red"` or `size = 3`.
#' @importFrom ggplot2 layer
#' @section Aesthetics:
#' \code{geom_point2()} understands the following aesthetics
#'     \itemize{
#'        \item \code{subset} logical expression indicating elements or rows to keep: missing values are taken as false; should be in aes().
#'        \item \code{colour} the colour of point, default is black.
#'        \item \code{fill} the colour of fill, default is black.
#'        \item \code{alpha} the transparency of fill, default is 1.
#'        \item \code{size} the size of point, default is 1.5.
#'        \item \code{shape} specify a shape, default is 19.
#'        \item \code{stroke} control point border thickness of point, default is 0.5.
#'     }
#' @seealso
#'  [geom_point][ggplot2::geom_point]; 
#'  [geom_rootpoint] add point of root; 
#'  [geom_nodepoint] add points of internal nodes; 
#'  [geom_tippoint] add points of external nodes (also known as tips or leaves).
#' @export
#' @return point layer
#' @author Guangchuang Yu 
#' @references 1. G Yu, DK Smith, H Zhu, Y Guan, TTY Lam (2017). ggtree: an R package for
#' visualization and annotation of phylogenetic trees with their covariates and
#' other associated data. Methods in Ecology and Evolution, 8(1):28-36.
#' <https://doi.org/10.1111/2041-210X.12628>
#' 
#' 2. G Yu*, TTY Lam, H Zhu, Y Guan*. Two methods for mapping and visualizing associated data 
#' on phylogeny using ggtree. Molecular Biology and Evolution, 2018, 35(2):3041-3043. <https://doi.org/10.1093/molbev/msy194>
#' 
#' 3. G Yu. Using ggtree to visualize data on tree-like structures. Current Protocols in 
#' Bioinformatics, 2020, 69:e96. <https://doi.org/10.1002/cpbi.96>
#' 
#' For more information about tree visualization, please refer to the online book
#' <https://yulab-smu.top/treedata-book/>
#' @export
#' @examples
#' library(ggtree)
#' ## add point by aes(subset)
#' tr <- rtree(10)
#' # group tip and node
#' ggtree(tr) + geom_point2(aes(shape=isTip, color=isTip), size=3)
#' # specify a node to display
#' ggtree(tr) + geom_point2(aes(subset=(node==15)), shape=21, size=5, fill='green')
#' # specify a tip to display
#' ggtree(tr) + geom_point2(aes(subset=(label %in% c("t1", "t3"))), shape=21, size=5, fill='green')
#' 
#' ## color point with continuous variables
#' library(ggtree)
#' library(treeio)
#' library(ggplot2)
#' beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
#' beast_tree <- read.beast(beast_file)
#' p <- ggtree(beast_tree) +
#'   geom_tiplab(hjust = -.1)+ 
#'   geom_nodepoint(aes(fill = rate), shape = 21, size = 4) +
#'   scale_fill_continuous(low = 'blue', high = 'red') +
#'   theme_tree2() + theme(legend.position = 'right')
#' p
#' 
geom_point2 <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  

    default_aes <- aes_() # node=~node)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPointGGtree,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        ),
        check.aes = FALSE
    )
}

##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 GeomPoint
##' @importFrom ggplot2 draw_key_point
GeomPointGGtree <- ggproto("GeomPointGGtree", GeomPoint,
                           setup_data = function(data, params) {
                               if (is.null(data$subset))
                                   return(data)
                               data[which(data$subset),]
                           }

                           ## ,

                           ## draw_panel = function(data, panel_scales, coord, na.rm = FALSE){
                           ##     GeomPoint$draw_panel(data, panel_scales, coord, na.rm)
                           ## },

                           ## draw_key = draw_key_point,

                           ## required_aes = c("x", "y"),
                           ## default_aes = aes(shape = 19, colour = "black", size = 1.5, fill = NA,
                           ##                   alpha = NA, stroke = 0.5)
                            )

