## Encircle code originally from:
## https://github.com/hrbrmstr/ggalt/blob/master/R/geom_encircle.r

### draw_key_hack

##' @importFrom scales alpha
##' @importFrom grid grobTree
##' @importFrom grid rectGrob
##' @importFrom grid gpar
draw_key_hack <- function(data, params, size) {
  print('draw_key_hack') ##DEBUG
  data$fill <- alpha(data$fill, data$alpha)
  data$alpha <- 1

  grobTree(
    if (!is.na(data$fill)) rectGrob(gp = gpar(col = NA, fill = data$fill)),
    draw_key_path(data, params)
  )
}

#' GeomHilight
#' @rdname ggtree-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 Geom
#' @export
GeomHilight <- ggproto("GeomHilight", Geom,
                       # Required fields in the 'data' data.frame for draw_group().
                       # Additional fields from the layer param field can be used and will be added to the data column.
                       # eg. required_aes = c("x", "y", "branch.length", "clade_root_node") will add "clade_root_node"
                       # to the 'data' data.frame passed to draw_{panel, group}()
                       required_aes = c("x", "y", "branch.length", "clade_root_node"),
                       # set default aesthetic parameters appended to 'data' data.frame
                       default_aes = aes(colour   = "black",
                                         fill     = "steelblue",
                                         alpha    = 0.5,
                                         expand   = 0.05,
                                         spread   = 0.1,
                                         linetype = 1,
                                         size     = 1,
                                         s_shape  = 0.5,  ## corresponds to default shape in xspline of -0.5
                                         s_open   = FALSE),

                       draw_key = draw_key_hack, ## ???

                       # # Find set of nodes that define the clade.
                       # setup_params = function(data, params) {
                       #   print('setup_params()') ## DEBUG
                       #   if (is.null(params$node)){
                       #     # Assume clade subset is given by user via data = data[subset]
                       #     return(params)
                       #   }
                       #
                       #   # Find set of child nodes from clade_node.
                       #   clade_node <- 15
                       #
                       #   #params$clade_root_node <- clade_node
                       #   params
                       # },

                       draw_group = function(data, panel_scales, coord) {
                         # Determine if tree is circular or radial as uses Polar coordinates.
                         #"CoordCartesian" %in% class(coord)
                         #"CoordPolar" %in% class(coord)

                         # Get clade root node and clade node ids.
                         clade_root_node <- data[1,]$clade_root_node

                         # Check if clade parent node exists in data.
                         if( !(clade_root_node %in% data$node) ){
                           cat('ERROR: clade node id (',clade_root_node,') not found in tree data.\n')
                           return(NULL)
                         }

                         clade_ids = ggtree:::getSubtree.df(data, clade_root_node)

                         # Remove non-clade rows.
                         data <- data[data$node %in% clade_ids,]

                         # # Get layout
                         #
                         # layout <- data[1,]$layout
                         #
                         # If layout is {"rectangular”, “slanted”, “fan”, “circular”, “radial”} then find set of points that define
                         # the retangular region around the clade.
                         # if( layout %in% c('rectangular', 'slanted', 'fan', 'circular', 'radial') ){
                         #
                         #   # get number of clade nodes.
                         #   n <- nrow(data)
                         #
                         #   # Find min and max (x,y) coordinates to find rectangle covering the clade.
                         #   X <- data$x
                         #   #Y <- data$y
                         #
                         #   min_x <- min(X)
                         #   max_x <- max(X)
                         #   #min_y <- min(Y)
                         #   #max_y <- max(Y)
                         #
                         #
                         #   # Start with single row
                         #   #data <- data[1,]
                         #   #data <- data[rep(seq_len(nrow(data)), 4), ]
                         #   #data$x <- c(max_x, min_x, min_x, max_x)
                         #   #data$y <- c(min_y, max_y, min_y, max_y)
                         #
                         #   points_right <- data
                         #   # Update points with bounded box (min and max of X )
                         #   data$x <- min_x
                         #   points_right$x <- max_x
                         #   print('points_right')
                         #   print(points_right)
                         #
                         #   # Combine left and right extreme points
                         #   data <- rbind(data, points_right)
                         #
                         #   print('Box data') #DEBUG
                         #   print(data) #DEBUG
                         #
                         # }

                         # Create glob
                         glob <- get_glob_encircle(data, panel_scales, coord)

                         return(glob)

                       }
)


get_glob_encircle <- function(data, panel_scales, coord){
  coords <- coord$transform(data, panel_scales)
  first_row <- coords[1, , drop = FALSE]
  rownames(first_row) <- NULL ## prevent warning later

  m <- lapply(coords[,c("x","y")],mean,na.rm=TRUE)
  ch <- grDevices::chull(coords[c("x","y")])

  mkcoords <- function(x,y) {
    data.frame(x,y,first_row[!names(first_row) %in% c("x","y")])
  }

  coords <- coords[ch,]
  ## FIXME: using grid:: a lot. importFrom instead?

  ## convert from lengths to physical units, for computing *directions*
  cc <- function(x,dir="x")
    grid::convertUnit(grid::unit(x,"native"),"mm",typeFrom="dimension",
                      axisFrom=dir,valueOnly=TRUE)

  ## convert back to native (e.g. native + snpc offset)
  cc_inv <- function(x,dir="x")
    grid::convertUnit(x,"native",typeFrom="location",
                      axisFrom=dir,valueOnly=TRUE)

  cc_comb <- function(x1,x2,dir="x")
    cc_inv(unit(x1,"native")+unit(x2,"snpc"),dir=dir)

  ## find normalized vector: d1 and d2 have $x, $y elements
  normFun <- function(d1,d2) {
    dx <- cc(d1$x-d2$x)
    dy <- cc(d1$y-d2$y)
    r <- sqrt(dx*dx+dy*dy)
    list(x=dx/r,y=dy/r)
  }

  if (nrow(coords)==1) {
    ## only one point: make a diamond by spreading points vertically
    ## and horizontally
    coords <- with(coords,
                   mkcoords(
                     c(x,x+spread,x,x-spread),
                     c(y+spread,y,y-spread,y)))
  } else if (nrow(coords)==2) {
    ## only two points: make a diamond by spreading points perpendicularly
    rot <- matrix(c(0,1,-1,0),2)
    dd <- c(rot %*% unlist(normFun(coords[1,],coords[2,])))*
      coords$spread
    coords <- with(coords, {
      ## figure out rotated values, then convert *back* to native units
      ## already in scaled units, so ignore?
      x <- c(x[1],
             m$x+dd[1], ## cc_comb(m$x,dd[1]),
             x[2],
             m$x-dd[1]) ## cc_comb(m$x,-dd[1]))
      y <- c(y[1],
             m$y+dd[2], ## cc_comb(m$y,dd[2],"y"),
             y[2],
             m$y-dd[2]) ## cc_comb(m$y,-dd[2],"y"))
      mkcoords(x,y)
    })
  }

  disp <- normFun(coords,m)

  ## browser()

  gp <- grid::get.gpar()
  pars1 <- c("colour","linetype","alpha","fill","size")
  pars2 <- c("col","lty","alpha","fill","lwd")
  gp[pars2] <- first_row[pars1]
  grid::xsplineGrob(
    with(coords,unit(x,"npc")+disp$x*unit(expand,"snpc")),
    with(coords,unit(y,"npc")+disp$y*unit(expand,"snpc")),
    ## coords$x,
    ## coords$y,
    shape = coords$s_shape-1,  ## kluge!
    open = first_row$s_open,
    gp = gp)

}



#' layer of hilight clade with xspline
#'
#' @title geom_hilight_encircle
#' @param data data frame to calculate xspline (default = NULL)
#' @param node selected node to hilight (required)
#' @param mapping aesthetic mapping (default = NULL)
#' @param fill colour fill (default = steelblue)
#' @param alpha alpha (transparency) (default = 0.5)
#' @param expand expands the xspline clade region only (default = 0)
#' @param ... addtional parameters, including:
#' 'spread' spread of shape? (default = 0.1),
#' 'linetype' Line type of xspline (default = 1),
#' 'size' Size of xspline line (default = 1),
#' 's_shape' Corresponds to shape of xspline (default = 0.5),
#' 's_open' Boolean switch determines if xspline shape is open or closed. (default = FALSE)
#' @return ggplot2
#' @export
#' @importFrom ggplot2 aes_
geom_hilight_encircle <- function(data = NULL,
                                  node,
                                  mapping     = NULL,
                                  fill        = 'steelblue',
                                  alpha       = 0.5,
                                  expand      = 0, # expand whole hilight region.
                                  ...) {

  position    = "identity"
  na.rm       = TRUE
  show.legend = NA
  inherit.aes = FALSE
  check.aes   = FALSE


  # Select fields(columns) from the ggtree "data" data.frame to be passed to the GeomHilight ggproto object.
  default_aes <- aes_( x=~x, y=~y, node=~node, parent=~parent, branch.length=~branch.length )

  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(mapping, default_aes)
  }

  # create xspline geom for non-uniform trees, e.g. unrooted layout
  l <- layer(
    geom = GeomHilight,
    stat = "identity",
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = check.aes,
    params = list(clade_root_node = node,
                  fill = fill,
                  alpha = alpha,
                  expand = expand,
                  na.rm = na.rm,
                  ...) # Parameters  to geom
  )

  return(l)

}
