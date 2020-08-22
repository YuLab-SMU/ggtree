#' display root edge
#'
#' `geom_rootedge` is used to create a rootedge. 
#' 
#' @title geom_rootedge
#' @param rootedge length of rootedge; use phylo$root.edge if rootedge = NULL (by default).
#' @param ... additional parameters
#' 
#' Additional parameters can be referred to the following parameters:
#'     \itemize{
#'         \item \code{size} control the width of rootedge, default is 0.5.
#'         \item \code{colour} color of rootedge, default is black.
#'         \item \code{linetype} the type of line, default is 1.
#'         \item \code{alpha} modify colour transparency, default is 1.
#'     }
#
#' @return ggtree rootedge layer
#' @export
#' @author Guangchuang Yu
#' @references 1. G Yu, DK Smith, H Zhu, Y Guan, TTY Lam (2017). ggtree: an R package for
#' visualization and annotation of phylogenetic trees with their covariates and
#' other associated data. Methods in Ecology and Evolution, 8(1):28-36.
#' <https://doi.org/10.1111/2041-210X.12628>
#' @export 
#' @examples 
#' \dontrun{
#' library(ggtree)
#' set.seed(123)
#' ## with root edge = 1
#' tree1 <- read.tree(text='((A:1,B:2):3,C:2):1;')
#' ggtree(tree1) + geom_tiplab() + geom_rootedge()
#' 
#' ## without root edge
#' tree2 <- read.tree(text='((A:1,B:2):3,C:2);')
#' ggtree(tree2) + geom_tiplab() + geom_rootedge()
#' 
#' ## setting root edge
#' tree2$root.edge <- 2
#' ggtree(tree2) + geom_tiplab() + geom_rootedge()
#' 
#' ## specify length of root edge for just plotting
#' ## this will ignore tree$root.edge
#' ggtree(tree2) + geom_tiplab() + geom_rootedge(rootedge = 3)
#' 
#' ## For more information about tree visualization, please refer to our online book
#' ## https://yulab-smu.top/treedata-book/chapter4.html
#' }
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
