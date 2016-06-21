##' hilight the two subclades of an internal node
##' 
##' Particularly useful when studying neighboring clades. Note that balances that
##' correspond to multichotomies will not be displayed.   
##' 
##' @title geom_balance
##' @param node selected node (balance) to hilight
##' @param fill color fill
##' @param color color to outline hilights and divide balance
##' @param alpha alpha (transparency)
##' @param extend extend xmax of the rectangle
##' @param extendto extend xmax to extendto
##' @return ggplot2
##' @export
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 GeomRect
##' @author Justin Silverman
geom_balance <- function(node, fill="steelblue", color='white', alpha=.5, extend=0, extendto=NULL) {
  
  
  data = NULL
  stat = "balance"
  position = "identity"
  show.legend = NA
  na.rm = TRUE
  inherit.aes = FALSE
  
  default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length)
  mapping <- default_aes
  
  
  l1 <- layer(
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
                  direction=1, 
                  na.rm = na.rm)
  )
  l2 <- layer(
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
                  direction=2,
                  na.rm = na.rm)
  )
  return(c(l1,l2))
}

##' stat_hilight
##'
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
stat_balance <- function(mapping=NULL, data=NULL, geom="rect",
                         position="identity",  node, 
                         show.legend=NA, inherit.aes=FALSE,
                         fill, color, alpha, extend=0, extendto=NULL,
                         ...) {
  default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length)
  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(mapping, default_aes)
  }
  
  l1 <- layer(
    stat=StatBalance,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    params = list(node=node,
                  fill=fill,
                  color=color,
                  alpha=alpha,
                  extend=extend,
                  extendto=extendto,
                  direction=1,
                  ...)
  )
  l2 <- layer(
    stat=StatBalance,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    params = list(node=node,
                  fill=fill,
                  color=color,
                  alpha=alpha,
                  extend=extend,
                  extendto=extendto,
                  direction=2,
                  ...)
  )
  return(c(l1,l2))
  
}

##' StatBalance
##' @rdname ggtree-ggproto
##' @format NULL
##' @usage NULL
##' @importFrom ggplot2 Stat
##' @export
StatBalance <- ggproto("StatBalance", Stat,
                       compute_group = function(self, data, scales, params, node, extend, extendto, direction) {
                         df <- get_balance_position_(data, node, direction)
                         
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


##' get position of balance (xmin, xmax, ymin, ymax)
##'
##'
##' @title get_balance_position
##' @param treeview tree view
##' @param node selected node
##' @param direction either (1 for 'up' or 2 for 'down')
##' @return data.frame
##' @export
##' @author Justin Silverman
get_balance_position <- function(treeview, node, direction) {
  get_balance_position_(treeview$data, node, direction)
}

get_balance_position_ <- function(data, node, direction) {
  ch <- tryCatch(getChild.df(data, node), error=function(e) NULL)
  
  i <- match(node, data$node)
  if (is.null(ch)) {
    ## tip
    stop('balance cannot be a tip')
  } else {
    sp <- tryCatch(get.offspring.df(data, ch[direction]), error=function(e) ch[direction])
    sp.all <- get.offspring.df(data, i)

    sp.df <- data[match(sp, data$node),]
    sp.all.df <- data[match(sp.all, data$node),]
    n.df <- data[i,]
  }
  
  # X direction is uniform for both children, but y is only based on range of
  # one of the two children (direction)
  x <- sp.all.df$x
  y <- sp.df$y
  #x.n <- n.df$x

  if ("branch.length" %in% colnames(data)) {
   xmin <- min(x)-data[i, "branch.length"]/2
  } else {
   xmin <- min(sp.df$branch)
  }
  #xmin <- x.n
  data.frame(#xmin=min(c(xmin,x.n)),
             xmin=xmin,
             #xmax=max(c(max(x), x.n)),
             xmax = max(x),
             ymin=min(y)-0.5,
             ymax=max(y)+0.5)
}

# get.offspring.df <- function (df, node) 
# {
#   sp <- getChild.df(df, node)
#   sp <- sp[sp != 0]
#   if (length(sp) == 0) {
#     stop("input node is a tip...")
#   }
#   i <- 1
#   while (i <= length(sp)) {
#     sp <- c(sp, getChild.df(df, sp[i]))
#     sp <- sp[sp != 0]
#     i <- i + 1
#   }
#   return(sp)
# }
# 
# getChild.df <- function (df, node) 
# {
#   i <- which(df$parent == node)
#   if (length(i) == 0) {
#     return(0)
#   }
#   res <- df[i, "node"]
#   res <- res[res != node]
#   return(res)
# }