#' highlights the two direct descendant clades of an internal node
#'
#' Particularly useful when studying neighboring clades. Note that balances that
#' correspond to multichotomies will not be displayed.
#'
#' @title geom_balance
#' @param node selected node (balance) to highlight its two direct descendant
#' @param fill color to fill in the highlight rectangle, default to "steelblue"
#' @param color color to outline highlight rectangle and divide balance, defaults to "white"
#' @param alpha alpha (transparency) for the highlight rectangle, defaults to 0.5
#' @param extend extend xmax of the highlight rectangle by the value of extend
#' @param extendto extend xmax of the highlight rectangle to the value of extendto
#' @return ggplot2
#' @export
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 GeomRect
#' @importFrom utils packageVersion
#' @author Justin Silverman and modified by Guangchuang Yu
#' @examples 
#' library(ggtree)
#' set.seed(123)
#' tr<- rtree(15)
#' x <- ggtree(tr)
#' x + geom_balance(17)
#' 
#' @references 
#' J. Silverman, et al. *A phylogenetic transform enhances
#' analysis of compositional microbiota data*. (in preparation)   
#'    
#' For more detailed demonstration, please refer to chapter 5.2.2 of 
#' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
#' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_balance <- function(node, fill="steelblue", color='white', alpha=.5, extend=0, extendto=NULL) {

  data = NULL
  stat = "balance"
  position = "identity"
  show.legend = NA
  na.rm = TRUE
  inherit.aes = FALSE

  default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length)
  mapping <- default_aes

  layer(
    stat=StatBalance,
    data = data,
    mapping = mapping,
    geom = GeomRect,
    position = position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    params = list(node=node,
                  fill=fill,
                  color=color,
                  alpha=alpha,
                  extend=extend,
                  extendto=extendto,
                  na.rm = na.rm),
    check.aes = FALSE
  )
}


##' StatBalance
##' @rdname ggtree-ggproto
##' @format NULL
##' @usage NULL
##' @importFrom ggplot2 Stat
##' @export
StatBalance <- ggproto("StatBalance", Stat,
                       compute_group = function(self, data, scales, params, node, extend, extendto) {
                           ## df <- get_balance_position_(data, node, direction)
                           df <- get_balance_position(data, node)
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


## get position of balance (xmin, xmax, ymin, ymax)
##
##
## @title get_balance_position
## @param data tbl_tree
## @param node selected node
## @return data.frame
## @export
get_balance_position <- function(data, node) {
    purrr::map_df(c(1, 2), get_balance_position_, data=data, node=node)
}

## direction either (1 for 'up' or 2 for 'down')
## @author Justin Silverman and modified by Guangchuang Yu
get_balance_position_ <- function(data, node, direction) {
    ## ch <- tryCatch(getChild.df(data, node), error=function(e) NULL)
    ch <- tryCatch(tidytree:::child.tbl_tree(data, node)$node, error=function(e) NULL)

    if (length(ch) < 2 || is.null(ch)){
        stop('balance cannot be a tip')
    } else if (length(ch) > 2){
        stop('balance has >2 direct child nodes, can use ape::multi2di to convert to binary tree')
    }

    i <- match(node, data$node)
    #sp <- tryCatch(tidytree:::offspring.tbl_tree(data, ch[direction])$node,
    #               error=function(e) ch[direction])
    sp <- tryCatch(offspring.tbl_tree(data, ch[direction])$node,error=function(e) ch[direction])
    if (length(sp) == 0) {
        ## sp is a tip, use itself
        sp <- ch[direction]
    }
    #sp.all <- tidytree:::offspring.tbl_tree(data, i)$node
    sp.all <- offspring.tbl_tree(data, i)$node
    sp.df <- data[match(sp, data$node),]
    sp.all.df <- data[match(sp.all, data$node),]
    n.df <- data[i,]

    ## X direction is uniform for both children, but y is only based on range of
    ## one of the two children (direction)
    x <- sp.all.df$x
    y <- sp.df$y
    ## x.n <- n.df$x

    if ("branch.length" %in% colnames(data)) {
        xmin <- min(x)-data[i, "branch.length"]/2
    } else {
        xmin <- min(sp.df$branch)
    }
    ## xmin <- x.n
    data.frame(xmin=xmin,
               xmax = max(x),
               ymin=min(y)-0.5,
               ymax=max(y)+0.5)
}
