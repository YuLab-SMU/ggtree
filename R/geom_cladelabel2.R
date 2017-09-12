##' annotate a clade with bar and text label
##'
##'
##' @title geom_cladelabel2
##' @param node selected node
##' @param label clade label
##' @param offset offset of bar and text from the clade
##' @param offset.text offset of text from bar
##' @param offset.bar offset of bar from text
##' @param align logical
##' @param barsize size of bar
##' @param fontsize font size of text
## @param angle angle of text
##' @param geom one of 'text' or 'label'
##' @param hjust justify text horizontally
##' @param color color for clade & label, of length 1 or 2
## @param fill fill label background, only work with geom='label'
##' @param family sans by default, can be any supported font
##' @param parse logical, whether parse label
##' @param ... additional parameter
##' @return ggplot layers
##' @export
##' @author JustGitting
geom_cladelabel2 <- function(node, label, offset=0, offset.text=0, offset.bar=0,
                            align=FALSE, barsize=0.5, fontsize=3.88, hjust = 0,
                            geom="text",
                            color = NULL,
                            family="sans", parse=FALSE, ...) {
    mapping <- NULL
    data <- NULL
    position <- "identity"
    show.legend <- NA
    na.rm <- TRUE
    inherit.aes <- FALSE


    # create custom arguments from ellipsis (aka '...') for stat_cladeText2 depending on geom type

    # http://ggplot2.tidyverse.org/reference/geom_text.html
    # geom_label(mapping = NULL, data = NULL, stat = "identity",
    #            position = "identity", ..., parse = FALSE, nudge_x = 0, nudge_y = 0,
    #            label.padding = unit(0.25, "lines"), label.r = unit(0.15, "lines"),
    #            label.size = 0.25, na.rm = FALSE, show.legend = NA,
    #            inherit.aes = TRUE)
    #
    # geom_text(mapping = NULL, data = NULL, stat = "identity",
    #           position = "identity", ..., parse = FALSE, nudge_x = 0, nudge_y = 0,
    #           check_overlap = FALSE, na.rm = FALSE, show.legend = NA,
    #           inherit.aes = TRUE)
    #
    # Aesthetics: x, y, label, alpha, angle, colour, family, fontface, group, hjust, lineheight, size, vjust

    # http://ggplot2.tidyverse.org/reference/geom_segment.html
    # geom_curve(mapping = NULL, data = NULL, stat = "identity",
    #            position = "identity", ..., curvature = 0.5, angle = 90, ncp = 5,
    #            arrow = NULL, lineend = "butt", na.rm = FALSE, show.legend = NA,
    #            inherit.aes = TRUE)
    #
    # Aesthetics:  x, y, xend, yend, alpha, colour, group, linetype, size

    # name_mapping = list('oldA'='newA', 'oldB'='newB')
    # data_list = list(oldB=1, oldA=2)
    # names(data_list) = name_mapping[match(names(data_list), names(name_mapping))]

    arg_list_geom_label <- c( "nudge_x", "nudge_y", "label.padding", "label.r", "label.size",
                              "alpha", "angle", "fontface", "group", "lineheight", "size", "vjust", "fill")

    arg_list_geom_text <- c( "nudge_x", "nudge_y", "check_overlap",
                             "alpha", "angle", "fontface", "group", "lineheight", "size", "vjust")

    # ignore angle
    arg_list_geom_curve <- c( "curvature", "ncp", "arrow", "lineend",
                              "alpha", "group", "linetype")


    # Parse ellipsis to collect parameters for geom_text or geom_label
    ellipsis <- list(...)
    if (geom == "text") {
      args_stat_cladeText2 <- ellipsis[names(ellipsis) %in% arg_list_geom_text]
    } else {
      args_stat_cladeText2 <- ellipsis[names(ellipsis) %in% arg_list_geom_label]
    }

    if (parse == 'emoji') {
      emoji <- get_fun_from_pkg("emojifont", "emoji")
      label <- emoji(label)
      parse <- FALSE
      family <- "EmojiOne"
    }


    # add parameters to stat_cladeText2 options.
    args_stat_cladeText2$node        <- node
    args_stat_cladeText2$label       <- label
    args_stat_cladeText2$offset      <- offset+offset.text
    args_stat_cladeText2$align       <- align
    args_stat_cladeText2$hjust       <- hjust
    args_stat_cladeText2$size        <- fontsize
    args_stat_cladeText2$family      <- family
    args_stat_cladeText2$mapping     <- mapping
    args_stat_cladeText2$data        <- data
    args_stat_cladeText2$geom        <- geom
    args_stat_cladeText2$position    <- position
    args_stat_cladeText2$show.legend <- show.legend
    args_stat_cladeText2$inherit.aes <- inherit.aes
    args_stat_cladeText2$na.rm       <- na.rm
    args_stat_cladeText2$parse       <- parse

    # create arg list of stat_cladeBar2.
    args_stat_cladeBar2 <- ellipsis[names(ellipsis) %in% arg_list_geom_curve]

    args_stat_cladeBar2$size        <- barsize
    args_stat_cladeBar2$node        <- node
    args_stat_cladeBar2$offset      <- offset+offset.bar
    args_stat_cladeBar2$align       <- align
    args_stat_cladeBar2$size        <- barsize
    args_stat_cladeBar2$mapping     <- mapping
    args_stat_cladeBar2$data        <- data
    args_stat_cladeBar2$position    <- position
    args_stat_cladeBar2$show.legend <- show.legend
    args_stat_cladeBar2$inherit.aes <- inherit.aes
    args_stat_cladeBar2$na.rm       <- na.rm


    if (!is.null(color)) {
        if (length(color) > 2) {
          stop("color should be of length 1 or 2")
        }
        if (length(color) == 0) {
          color = NULL
        } else if (length(color) == 1) {
          args_stat_cladeText2$colour <- color
          args_stat_cladeBar2$colour <- color
        } else {
          args_stat_cladeText2$colour <- color[1]
          args_stat_cladeBar2$colour <- color[2]
        }
    }

    # print('text opts') # Debug
    # print(args_stat_cladeText2) # Debug
    # print('bar opts') # Debug
    # print(args_stat_cladeBar2) # Debug

    # create text and bar layers.
    layer_text <- do.call(stat_cladeText2, args_stat_cladeText2)
    layer_bar <- do.call(stat_cladeBar2, args_stat_cladeBar2)

    list(
      layer_bar,
      layer_text
    )
}

# Display label at middle angle of clade subtree arc.
stat_cladeText2 <- function(mapping=NULL, data=NULL,
                            geom="text", position="identity",
                            node, label, offset, align, ...,
                            show.legend=NA, inherit.aes=FALSE,
                            na.rm=FALSE, parse=FALSE) {
  # columns from ggplot data data.frame.
  default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent)
  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(mapping, default_aes)
  }

  layer(stat=StatCladeText2,
        data=data,
        mapping=mapping,
        geom=geom,
        position=position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        check.aes = FALSE,
        params=list(node=node,
                    label  = label,
                    offset = offset,
                    align  = align,
                    na.rm  = na.rm,
                    parse  = parse,
                    ...)

  )

}

stat_cladeBar2 <- function(mapping=NULL, data=NULL,
                           geom="curve", position="identity",
                           node, offset, align, ...,
                           show.legend=NA, inherit.aes=FALSE, na.rm=FALSE) {
  default_aes <- aes_(x=~x, y=~y, node=~node, parent=~parent, xend=~x, yend=~y)
  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(mapping, default_aes)
  }

  layer(stat=StatCladeBar2,
        data=data,
        mapping=mapping,
        geom=geom,
        position=position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        check.aes = FALSE,
        params=list(node=node,
                    offset=offset,
                    align=align,
                    na.rm=na.rm,
                    ...)

  )
}

StatCladeText2 <- ggproto("StatCladeText2", Stat,

                          required_aes = c("x", "y", "label"),

                          compute_group = function(self, data, scales, params = NULL, node, label, offset, align) {
                            df <- get_cladelabel2_position_label(data, node, offset, align, adjustRatio = 1.2)

                            # computer_group does not need to return df$label as label is declared in the geom_cladelabel2() function.
                            # The data.frame returned by computer_group() does not override the variables explicitly specified in the geom_cladelabel2()
                            # df$label <- label

                            if(is.null(params$angle)){
                              df$angle <- df$theta_label * 180
                              if( df$angle > 90 & df$angle < 270){
                                # add 180 to angle so label is easy to ready
                                df$angle <- df$angle + 180
                              }
                            }

                            return(df)
                          }
)

StatCladeBar2 <- ggproto("StatCladeBar2", Stat,

                        required_aes = c("x", "y", "xend", "yend"),

                        compute_group = function(self, data, scales, params, node, offset, align) {
                          df <- get_cladelabel2_position_bar(data, node, offset, align, adjustRatio=1.1)
                          return(df)
                        }
)

get_cladelabel2_position_label <- function(data, node, offset, align, adjustRatio) {
  df <- get_cladelabel2_position_(data, node)

  if (align) {
    # Find max radius from tree root.
    r <- max(getNodeEuclDistances(data, getRoot.df(data)))
  } else {
    r <- df$r
  }

  r <- r * adjustRatio + offset

  # Calculate the angle between theta_left and theta_right
  delta <- df$theta_left - df$theta_right

  if(delta > 0){
    theta_label <- delta/2 + df$theta_right
  }else if(delta < 0){
    delta_adj <- delta + 2
    theta_label <- delta_adj/2 + df$theta_right
  }else{
    theta_label <- df$theta_left
  }

  # correct if theta_label > 360
  if(theta_label > 2){
    theta_label <-  theta_label - 2
  }

  # Calculate the position of the label
  x1 <- r*cospi(theta_label) + data[data$node==node, 'x']
  y1 <- r*sinpi(theta_label) + data[data$node==node, 'y']

  data.frame(x=x1, y=y1, theta_label=theta_label)

}


get_cladelabel2_position_bar <- function(data, node, offset, align, adjustRatio) {
  df <- get_cladelabel2_position_(data, node)

  if (align) {
    # Find max radius from tree root.
    r <- max(getNodeEuclDistances(data, getRoot.df(data)))
  } else {
    r <- df$r
  }

  r <- r * adjustRatio + offset

  # Calculate the left(end) and right(start) points for the arc
  x1 <- r*cospi(df$theta_right) + data[data$node==node, 'x']
  y1 <- r*sinpi(df$theta_right) + data[data$node==node, 'y']
  xend <- r*cospi(df$theta_left) + data[data$node==node, 'x']
  yend <- r*sinpi(df$theta_left) + data[data$node==node, 'y']

  data.frame(x=x1, y=y1, xend=xend, yend=yend)

}

# Get clade subtree arc angles and maximum radius from clade node to all other clade nodes.
get_cladelabel2_position_ <- function(data, node) {
  # get left and right angles of the clade subtree.
  subtree <- list( subtree = getSubtree.df(data, node), node = node )

  arc <- getTreeArcAngles(data, node, subtree)
  # get max distance from node to clade tips.
  r <- max(getNodeEuclDistances(data[data$node %in% subtree$subtree,], node))

  data.frame(r=r, theta_left=as.numeric(arc['left']), theta_right=as.numeric(arc['right']))
}

