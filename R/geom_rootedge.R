#' display root edge
#'
#' `geom_rootedge` is used to create a rootedge. \cr
#' 
#' Additional parameters can refer the following parameters
#'     \itemize{
#'         \item \code{rootedge} length of rootedge, default is NULL.
#'         \item \code{size} control the width of rootedge, default is 0.5.
#'         \item \code{colour} color of rootedge, default is black.
#'         \item \code{linetype} the type of line, default is 1.
#'         \item \code{alpha} modify colour transparency, default is 1.
#'     }
#' @title geom_rootedge
#' @param rootedge length of rootedge; use phylo$root.edge if rootedge = NULL (by default).
#' @param ... additional parameters
#' @return ggplot layer
#' @export
#' @author Guangchuang Yu
#' @references 1. G Yu, DK Smith, H Zhu, Y Guan, TTY Lam (2017). ggtree: an R package for
#' visualization and annotation of phylogenetic trees with their covariates and
#' other associated data. Methods in Ecology and Evolution, 8(1):28-36.
#' <https://doi.org/10.1111/2041-210X.12628>
#' @export 
#' @examples 
#' library(ggtree)
#' set.seed(123)
#' tr <- rtree(8)
#' p <- ggtree(tr)
#' ## length of rootedge is 1  
#' p + geom_rootedge(rootedge = 1)
#' ## a red rootedge 
#' p +  geom_rootedge(rootedge = 1, color = "red")
geom_rootedge <- function(rootedge = NULL, ...) {
    mapping <- aes_(x = ~x, y = ~y, xend = ~x, yend = ~y,
                    branch.length = ~branch.length,
                    node = ~node, parent = ~parent)
    layer(
        stat = StatRootEdge,
        data  = NULL,
        mapping = mapping,
        geom = "segment",
        position = "identity",
        show.legend = NA,
        params = list(rootedge = rootedge, ...),
        check.aes = FALSE,
        inherit.aes = FALSE
    )

}



StatRootEdge <- ggproto("StatRootEdge", Stat,
                        compute_group = function(self, data, scales, params, rootedge) {
                            d <- data[data$parent == data$node,]
                            if (is.null(rootedge)) {
                                rootedge <- d$branch.length
                            }
                            if (is.null(rootedge)) {
                                xend <- d$x
                            } else if (is.na(rootedge)) {
                                xend <- d$x
                            } else {
                                xend <- d$x - rootedge
                            }

                            data.frame(x = d$x, y = d$y,
                                       xend = xend, yend = d$y)
                        },
                        required_aes = c("x", "y", "xend", "yend")
                        )
