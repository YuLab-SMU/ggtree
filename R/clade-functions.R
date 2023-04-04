##' get taxa name of a selected node (or tree if node=NULL) sorted by their position in plotting
##'
##'
##' This function extract an ordered vector of the tips from selected clade or the whole tree
##' based on the ggtree() plot. 
##' @title get_taxa_name
##' @param tree_view tree view (i.e. the ggtree object). If tree_view is NULL, the last ggplot object will be used.
##' @param node internal node number to specify a clade. If NULL, using the whole tree
##' @return ordered taxa name vector
##' @importFrom tidytree offspring
##' @export
##' @examples
##' tree <- rtree(30)
##' p <- ggtree(tree)
##' get_taxa_name(p)
##' @author Guangchuang Yu
get_taxa_name <- function(tree_view=NULL, node=NULL) {
    tree_view %<>% get_tree_view

    df <- tree_view$data
    if (!is.null(node)) {
        ## sp <- get.offspring.df(df, node)
        ## df <- df[sp, ]
        df <- offspring(df, node)
    }

    ## dplyr::filter(.data$isTip) %>%
    ##     dplyr::arrange(dplyr::desc(.data$y)) %>%
    ##     dplyr::pull(.data$label)

    with(df, {
        i = order(y, decreasing=T)
        label[i][isTip[i]]
    })
}


##' view a selected clade of tree, clade can be selected by specifying a node number or 
##' determined by the most recent common ancestor of selected tips
##'
##'
##' @title viewClade
##' @inheritParams get_taxa_name
##' @param xmax_adjust adjust the max range of x axis
##' @return clade plot
##' @importFrom ggplot2 ggplot_build
##' @importFrom ggplot2 coord_cartesian
##' @importFrom aplot xrange
##' @export
##' @examples
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab()
##' viewClade(p, 18, xmax_adjust = 0.)
##' @author Guangchuang Yu
viewClade <- function(tree_view=NULL, node, xmax_adjust=0) {
    tree_view %<>% get_tree_view
    ## xd <- tree_view$data$branch.length[node]/2

    cpos <- get_clade_position(tree_view, node=node)
    xmax <- xrange(tree_view)[2]

    attr(tree_view, 'viewClade') <- TRUE
    attr(tree_view, 'viewClade_node') <- node

    ## tree_view+xlim(cpos$xmin, xmax + xmax_adjust) + ylim(cpos$ymin, cpos$ymax)
    tree_view + coord_cartesian(xlim=c(cpos$xmin, xmax + xmax_adjust),
                                ylim=c(cpos$ymin, cpos$ymax), expand=FALSE)
}

is.viewClade <- function(tree_view) {
    x <- attr(tree_view, 'viewClade')
    !is.null(x) && x
}




##' collapse a selected clade, which can later be expanded with the 'expand()' fuction if necessary
##'
##'
##' @title collapse-ggtree
##' @rdname collapse
##' @param x tree view (i.e. the ggtree object). If tree_view is NULL, the last ggplot will be used.
##' @param node internal node number 
##' @param mode one of 'none'(default), 'max', 'min' and 'mixed'. 'none' would simply collapse the clade as 'tip' and 
##' the rest will display a triangle, whose shape is determined by the farest/closest tip of the collapsed clade to indicate it
##' @param clade_name set a name for the collapsed clade. If clade_name = NULL, do nothing
##' @param ... additional parameters to set the color or transparency of the triangle
##' @return tree view
##' @method collapse ggtree
##' @importFrom ggplot2 geom_polygon
##' @export
##' @examples
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab()
##' p
##' p1 <- collapse(p, node = 17, mode = "mixed", clade_name = "cclade", alpha = 0.8, color = "grey", fill = "light blue")
##' @seealso expand
##' @author Guangchuang Yu
collapse.ggtree <- function(x=NULL, node, mode = "none", clade_name = NULL, ...) {
    tree_view <- get_tree_view(x)
    mode <- match.arg(mode, c("none", "max", "min", "mixed"))

    df <- tree_view$data

    if (is.na(df$x[df$node == node])) {
        warning("specific node was already collapsed...")
        return(tree_view)
    }

    ## sp <- get.offspring.df(df, node)
    ## sp.df <- df[sp,]
    sp.df <- offspring(df, node)
    if (nrow(sp.df) == 0) {
        warning("input node is a tip...")
        return(tree_view)
    }

    if (mode == "none") {
        ## df[node, "isTip"] <- TRUE
        sp_y <- range(sp.df$y, na.rm=TRUE)
        ii <- which(df$y > max(sp_y))
        if (length(ii)) {
            df$y[ii] <- df$y[ii] - diff(sp_y)
        }
        df$y[node] <- min(sp_y)
        
        df[sp.df$node, "x"] <- NA
        df[sp.df$node, "y"] <- NA

        df <- reassign_y_from_node_to_root(df, node)
        
        ## re-calculate branch mid position
        df <- calculate_branch_mid(df, layout=get_layout(tree_view))

        ii <- which(!is.na(df$x))
        df$angle[ii] <- calculate_angle(df[ii,])$angle
    } else {
        ## reference https://jean.manguy.eu/subtrees-as-triangles-with-ggtree/
 
        sp_coord <- dplyr::summarise(sp.df[sp.df$isTip,],
                                     xmax = max(.data$x),
                                     xmin = min(.data$x),
                                     ymax = max(.data$y),
                                     ymin = min(.data$y))
 
        triangle <- switch(
            mode,
            max = tibble::tibble(
                x = c(df$x[node], sp_coord$xmax, sp_coord$xmax),
                y = c(df$y[node], sp_coord$ymin, sp_coord$ymax)
            ),
            min = tibble::tibble(
                x = c(df$x[node], sp_coord$xmin, sp_coord$xmin),
                y = c(df$y[node], sp_coord$ymin, sp_coord$ymax)
            ),
            mixed = tibble::tibble(
                x = c(df$x[node], sp_coord$xmin, sp_coord$xmax),
                y = c(df$y[node], sp_coord$ymin, sp_coord$ymax)                
            )
        )

        df[sp.df$node, "x"] <- NA
        df[sp.df$node, "y"] <- NA
    }

    ## set clade name
    if (!is.null(clade_name))
        df$label[node] <- clade_name

    tree_view$data <- df

    if (mode != "none") {
        tree_view <- tree_view +
            geom_polygon(mapping = aes_(x = ~x, y = ~y),
                         data = triangle, inherit.aes = FALSE, ...)
    }

    clade <- paste0("collapse_clade_", node)
    mode_attr <- paste0("collapse_mode_", node)
    attr(tree_view, clade) <- sp.df
    attr(tree_view, mode_attr) <- mode

    tree_view
}

##' expand collapsed clade
##'
##'
##' @title expand
##' @inheritParams get_taxa_name
##' @return tree view
##' @export
##' @examples
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab()
##' p1 <- collapse(p, 17)
##' expand(p1, 17)
##' @seealso collapse
##' @author Guangchuang Yu
expand <- function(tree_view=NULL, node) {
    tree_view %<>% get_tree_view

    clade <- paste0("collapse_clade_", node)
    sp.df <- attr(tree_view, clade)
    mode_attr <- paste0("collapse_mode_", node)
    mode <- attr(tree_view, mode_attr)

    if (is.null(sp.df)) {
        return(tree_view)
    }
    df <- tree_view$data

    if (mode == "none") {
        ## df[node, "isTip"] <- FALSE
        sp_y <- range(sp.df$y)
        ii <- which(df$y > df$y[node])
        df[ii, "y"] <- df[ii, "y"] + diff(sp_y)

        sp.df$y <- sp.df$y - min(sp.df$y) + df$y[node]
        df[sp.df$node,] <- sp.df
        
        root <- which(df$node == df$parent)
        pp <- node
        while(any(pp != root)) {
            ## df[pp, "y"] <- mean(df$y[getChild.df(df, pp)])
            df[pp, "y"] <- mean(tidytree::child(df, pp)$y)
            pp <- df$parent[pp]
        }
        ## j <- getChild.df(df, pp)
        j <- tidytree::child(df, pp)$node
        j <- j[j!=pp]
        df[pp, "y"] <- mean(df$y[j])
        
        ## re-calculate branch mid position
        df <- calculate_branch_mid(df, layout=get_layout(tree_view))

        tree_view$data <- calculate_angle(df)
    } else {
        df[sp.df$node,] <- sp.df
        tree_view$data <- df
    }

    attr(tree_view, clade) <- NULL
    attr(tree_view, mode_attr) <- NULL
    return(tree_view)
}

##' rotate selected clade by 180 degree
##'
##'
##' @title rotate
##' @inheritParams get_taxa_name
##' @return ggplot2 object
##' @export
##' @examples
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab()
##' rotate(p, 17)
##' @author Guangchuang Yu
rotate <- function(tree_view=NULL, node) {
    tree_view %<>% get_tree_view

    df <- tree_view$data
    ## sp <- get.offspring.df(df, node)
    ## sp_idx <- with(df, match(sp, node))
    ## tip <- sp[df$isTip[sp_idx]]
    ## sp.df <- df[sp_idx,]
    sp.df <- offspring(df, node)
    sp <- sp.df$node
    sp_idx <- with(df, match(sp, node))
    tip <- sp[df$isTip[sp_idx]]

    ii <- with(sp.df, match(tip, node))
    jj <- ii[order(sp.df$y[ii])]
    sp.df[jj,"y"] <- rev(sp.df$y[jj])
    sp.df[-jj, "y"] <- NA
    sp.df <- re_assign_ycoord_df(sp.df, tip)

    df[sp_idx, "y"] <- sp.df$y
    ## df$node == node is TRUE when node was root
    df[df$node == node, "y"] <- mean(df$y[df$parent == node & df$node != node])
    pnode <- df$parent[df$node == node]
    if (pnode != node && !is.na(pnode)) {
        df[df$node == pnode, "y"] <- mean(df$y[df$parent == pnode])
    }

    tree_view$data <- calculate_angle(df)
    tree_view
}



##' exchange the position of 2 clades
##'
##'
##' @title flip
##' @param tree_view tree view (i.e. the ggtree object). If tree_view is NULL, the last ggplot will be used.
##' @param node1 node number of clade 1. It should share a same parent node with node2
##' @param node2 node number of clade 2. It should share a same parent node with node1
##' @return ggplot object
##' @export
##' @examples
##' set.seed(123)
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab() +
##'   geom_nodelab(aes(subset=!isTip, label=node), hjust = -.1, color = "red")
##' flip(p, 23, 24)   ## Depends on the condition of your tree
##' @author Guangchuang Yu
flip <- function(tree_view=NULL, node1, node2) {
    tree_view %<>% get_tree_view

    df <- tree_view$data
    p1 <- with(df, parent[node == node1])
    p2 <- with(df, parent[node == node2])

    if (p1 != p2) {
        stop("node1 and node2 should share a same parent node...")
    }

    ## sp1 <- c(node1, get.offspring.df(df, node1))
    ## sp2 <- c(node2, get.offspring.df(df, node2))

    ## sp1.df <- df[sp1,]
    ## sp2.df <- df[sp2,]

    sp1.df <- offspring(df, node1, self_include = TRUE)
    sp2.df <- offspring(df, node2, self_include = TRUE)
    sp1 <- sp1.df$node
    sp2 <- sp2.df$node

    min_y1 <- min(sp1.df$y, na.rm=TRUE)
    min_y2 <- min(sp2.df$y, na.rm=TRUE)

    if (min_y1 < min_y2) {
        tmp <- sp1.df
        sp1.df <- sp2.df
        sp2.df <- tmp
        tmp <- sp1
        sp1 <- sp2
        sp2 <- tmp
    }

    min_y1 <- min(sp1.df$y, na.rm=TRUE)
    min_y2 <- min(sp2.df$y, na.rm=TRUE)

    space <- min(sp1.df$y, na.rm=TRUE) - max(sp2.df$y, na.rm=TRUE)
    sp1.df$y <- sp1.df$y - abs(min_y1 - min_y2)
    sp2.df$y <- sp2.df$y + max(sp1.df$y, na.rm=TRUE) + space - min(sp2.df$y, na.rm=TRUE)


    df[sp1, "y"] <- sp1.df$y
    df[sp2, "y"] <- sp2.df$y

    ## yy <- df$y[-c(sp1, sp2)]
    ## df$y[-c(sp1, sp2)] <- yy + ((min(sp2.df$y, na.rm=TRUE) - max(yy)) - (min(yy) - max(sp1.df$y, na.rm=TRUE)))/2

    anc <- ancestor(df, node1)$node
    ii <- match(anc, df$node)
    df[ii, "y"] <- NA
    ## currentNode <- unlist(as.vector(sapply(anc, getChild.df, df=df)))
    currentNode <- unlist(as.vector(sapply(anc, function(.node) tidytree::child(df, .node)$node)))
    currentNode <- currentNode[!currentNode %in% anc]

    tree_view$data <- re_assign_ycoord_df(df, currentNode)
    tree_view$data <- calculate_angle(tree_view$data)
    tree_view
}


##' zoom out/in a selected clade to emphasize or de-emphasize it
##'
##'
##' @title scaleClade
##' @inheritParams get_taxa_name
##' @param scale the scale of the selected clade. The clade will be zoom in when scale > 1,
##' and will be zoom out when scale < 1
##' @param vertical_only logical. If TRUE (default), only vertical will be scaled.
##' If FALSE, the clade will be scaled vertical and horizontally.
##' @return tree view
##' @export
##' @examples
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab() +
##'   geom_nodelab(aes(subset=!isTip, label=node), hjust = -.1, color = "red")
##' scaleClade(p, 24, scale = .1)
##' @author Guangchuang Yu
scaleClade <- function(tree_view=NULL, node, scale=1, vertical_only=TRUE) {
    tree_view %<>% get_tree_view

    if (scale == 1) {
        return(tree_view)
    }

    df <- tree_view$data
    ## sp <- get.offspring.df(df, node)
    ## sp.df <- df[sp,]
    sp.df <- offspring(df, node)
    sp <- sp.df$node
    
    ## sp_nr <- nrow(sp.df)
    ## span <- diff(range(sp.df$y))/sp_nr

    ## new_span <- span * scale
    old.sp.df <- sp.df
    sp.df$y <- df$y[node] + (sp.df$y - df$y[node]) * scale
    if (! vertical_only) {
        sp.df$x <- df$x[node] + (sp.df$x - df$x[node]) * scale
    }

    scale_diff.up <- max(sp.df$y) - max(old.sp.df$y)
    scale_diff.lw <- min(sp.df$y) - min(old.sp.df$y)

    ii <- df$y > max(old.sp.df$y)
    if (sum(ii) > 0) {
        df[ii, "y"] <- df$y[ii] + scale_diff.up
    }

    jj <- df$y < min(old.sp.df$y)
    if (sum(jj) > 0) {
        df[jj, "y"] <- df$y[jj] + scale_diff.lw
    }

    df[sp,] <- sp.df

    if (! "scale" %in% colnames(df)) {
        df$scale <- 1
    }
    df[sp, "scale"] <- df[sp, "scale"] * scale

    df <- reassign_y_from_node_to_root(df, node)

    ## re-calculate branch mid position
    df <- calculate_branch_mid(df, layout=get_layout(tree_view))

    tree_view$data <- calculate_angle(df)


    if (is.viewClade(tree_view)) {
        vc_node <- attr(tree_view, 'viewClade_node')
        tree_view <- viewClade(tree_view, vc_node)
    }

    tree_view
}



reassign_y_from_node_to_root <- function(df, node) {
    root <- which(df$node == df$parent)
    pp <- df$parent[node]
    while(any(pp != root)) {
        ## df[pp, "y"] <- mean(df$y[getChild.df(df, pp)])
        df[pp, "y"] <- mean(tidytree::child(df, pp)$y)
        pp <- df$parent[pp]
    }
    ## j <- getChild.df(df, pp)
    j <- tidytree::child(df, pp)$node
    j <- j[j!=pp]
    df[pp, "y"] <- mean(df$y[j])
    return(df)
}


##' zoom in on a selected clade of a tree, while showing its on the full view of tree as a seperated panel for reference
##'
##' 
##' @title zoomClade
##' @inheritParams get_taxa_name
##' @param xexpand numeric, expend the xlim of the zoom area.
##' default is NULL.
##' @return full tree with zoom in clade
##' @author Guangchuang Yu
##' @examples
##' \dontrun{
##' x <- rtree(15)
##' p <- ggtree(x) + geom_tiplab() +
##'   geom_nodelab(aes(subset=!isTip, label=node), hjust = -.1, color = "red")
##' zoomClade(p, 21, xexpand = .2)
##' }
##' @export
zoomClade <- function(tree_view = NULL, node, xexpand=NULL) {
    p <- get_tree_view(tree_view)
    sp <- offspring(p, node, self_include=TRUE)
    xr <- range(sp$x)
    if (is.null(xexpand)){
        xr[2] <- xr[2] + diff(xr)/10
    }else{
        xr[2] <- xr[2] + diff(xr)/10 + xr[2]*xexpand
    }
    yr <- range(sp$y)
    ## nn <- sp$node
    ## p + ggforce::facet_zoom(y = node %in% nn, ylim = yr)
    facet_zoom <- getFromNamespace("facet_zoom", "ggforce")

    p + facet_zoom(xlim = xr, ylim=yr)
}
