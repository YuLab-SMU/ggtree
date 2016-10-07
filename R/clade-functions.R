##' get taxa name of a selected node
##'
##' 
##' @title get_taxa_name
##' @param tree_view tree view
##' @param node node
##' @return taxa name vector
##' @export
##' @author Guangchuang Yu
get_taxa_name <- function(tree_view=NULL, node) {
    tree_view %<>% get_tree_view
    
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    res <- df[sp, "label"]
    return(res[df[sp, "isTip"]])
}




##' view a clade of tree
##'
##' 
##' @title viewClade
##' @param tree_view full tree view 
##' @param node internal node number
##' @param xmax_adjust adjust xmax
##' @return clade plot
##' @export
##' @author Guangchuang Yu
viewClade <- function(tree_view=NULL, node, xmax_adjust=0) {
    tree_view %<>% get_tree_view
    
    cpos <- get_clade_position(tree_view, node=node)
    with(cpos, tree_view+xlim(xmin, xmax*1.01 + xmax_adjust) + ylim(ymin, ymax))
}


##' collapse a clade
##'
##' 
##' @title collapse
##' @param tree_view tree view 
##' @param node clade node
##' @return tree view
##' @export
##' @seealso expand
##' @author Guangchuang Yu
collapse <- function(tree_view=NULL, node) {    
    tree_view %<>% get_tree_view
    
    df <- tree_view$data

    if (is.na(df$x[df$node == node])) {
        warning("specific node was already collapsed...")
        return(tree_view)
    }
    
    sp <- get.offspring.df(df, node)
    sp.df <- df[sp,]
    df[node, "isTip"] <- TRUE
    sp_y <- range(sp.df$y, na.rm=TRUE)
    ii <- which(df$y > max(sp_y))
    if (length(ii)) {
        df$y[ii] <- df$y[ii] - diff(sp_y)
    }
    df$y[node] <- min(sp_y)

    df[sp, "x"] <- NA
    df[sp, "y"] <- NA
    
    df <- reassign_y_from_node_to_root(df, node)
    
    ## re-calculate branch mid position
    df <- calculate_branch_mid(df)

    ii <- which(!is.na(df$x))
    df$angle[ii] <- calculate_angle(df[ii,])$angle
    
    tree_view$data <- df
    clade <- paste0("clade_", node)
    attr(tree_view, clade) <- sp.df
    tree_view
}

##' expand collased clade
##'
##' 
##' @title expand
##' @param tree_view tree view
##' @param node clade node
##' @return tree view
##' @export
##' @seealso collapse
##' @author Guangchuang Yu
expand <- function(tree_view=NULL, node) {
    tree_view %<>% get_tree_view
    
    clade <- paste0("clade_", node)
    sp.df <- attr(tree_view, clade)
    if (is.null(sp.df)) {
        return(tree_view)
    }
    df <- tree_view$data
    df[node, "isTip"] <- FALSE
    sp_y <- range(sp.df$y)
    ii <- which(df$y > df$y[node])
    df[ii, "y"] <- df[ii, "y"] + diff(sp_y)
    
    sp.df$y <- sp.df$y - min(sp.df$y) + df$y[node]
    df[sp.df$node,] <- sp.df

    root <- which(df$node == df$parent)
    pp <- node
    while(any(pp != root)) {
        df[pp, "y"] <- mean(df[getChild.df(df, pp), "y"])
        pp <- df[pp, "parent"]
    }
    j <- getChild.df(df, pp)
    j <- j[j!=pp]
    df[pp, "y"] <- mean(df[j, "y"])

    ## re-calculate branch mid position
    df <- calculate_branch_mid(df)
    
    tree_view$data <- calculate_angle(df)
    attr(tree_view, clade) <- NULL
    tree_view
}

##' rotate 180 degree of a selected branch
##'
##' 
##' @title rotate
##' @param tree_view tree view 
##' @param node selected node
##' @return ggplot2 object
##' @export
##' @author Guangchuang Yu
rotate <- function(tree_view=NULL, node) {
    tree_view %<>% get_tree_view
    
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    sp_idx <- with(df, match(sp, node))
    tip <- sp[df$isTip[sp_idx]]
    sp.df <- df[sp_idx,]
    ii <- with(sp.df, match(tip, node))
    jj <- ii[order(sp.df[ii, "y"])]
    sp.df[jj,"y"] <- rev(sp.df[jj, "y"])
    sp.df[-jj, "y"] <- NA
    sp.df <- re_assign_ycoord_df(sp.df, tip)

    df[sp_idx, "y"] <- sp.df$y
    ## df$node == node is TRUE when node was root
    df[df$node == node, "y"] <- mean(df[df$parent == node & df$node != node, "y"])
    pnode <- df$parent[df$node == node]
    if (pnode != node && !is.na(pnode)) {
        df[df$node == pnode, "y"] <- mean(df[df$parent == pnode, "y"])
    }

    tree_view$data <- calculate_angle(df)
    tree_view
}



##' flip position of two selected branches
##'
##' 
##' @title flip
##' @param tree_view tree view 
##' @param node1 node number of branch 1
##' @param node2 node number of branch 2
##' @return ggplot2 object
##' @export
##' @author Guangchuang Yu
flip <- function(tree_view=NULL, node1, node2) {
    tree_view %<>% get_tree_view
    
    df <- tree_view$data
    p1 <- with(df, parent[node == node1])
    p2 <- with(df, parent[node == node2])

    if (p1 != p2) {
        stop("node1 and node2 should share a same parent node...")
    }

    sp1 <- c(node1, get.offspring.df(df, node1))
    sp2 <- c(node2, get.offspring.df(df, node2))

    sp1.df <- df[sp1,]
    sp2.df <- df[sp2,]

    min_y1 <- min(sp1.df$y)
    min_y2 <- min(sp2.df$y)

    if (min_y1 < min_y2) {
        tmp <- sp1.df
        sp1.df <- sp2.df
        sp2.df <- tmp
        tmp <- sp1
        sp1 <- sp2
        sp2 <- tmp
    }

    min_y1 <- min(sp1.df$y)
    min_y2 <- min(sp2.df$y)

    space <- min(sp1.df$y) - max(sp2.df$y)
    sp1.df$y <- sp1.df$y - abs(min_y1 - min_y2)
    sp2.df$y <- sp2.df$y + max(sp1.df$y) + space - min(sp2.df$y)

    df[sp1, "y"] <- sp1.df$y
    df[sp2, "y"] <- sp2.df$y

    anc <- getAncestor.df(df, node1)
    ii <- match(anc, df$node)
    df[ii, "y"] <- NA
    currentNode <- unlist(as.vector(sapply(anc, getChild.df, df=df)))
    currentNode <- currentNode[!currentNode %in% anc]
    
    tree_view$data <- re_assign_ycoord_df(df, currentNode)
    tree_view$data <- calculate_angle(tree_view$data)
    tree_view
}


##' scale clade
##'
##' 
##' @title scaleClade
##' @param tree_view tree view
##' @param node clade node
##' @param scale scale
##' @param vertical_only logical. If TRUE, only vertical will be scaled.
##' If FALSE, the clade will be scaled vertical and horizontally.
##' TRUE by default.
##' @return tree view
##' @export
##' @author Guangchuang Yu
scaleClade <- function(tree_view=NULL, node, scale=1, vertical_only=TRUE) {
    tree_view %<>% get_tree_view
    
    if (scale == 1) {
        return(tree_view)
    }
    
    df <- tree_view$data
    sp <- get.offspring.df(df, node)
    sp.df <- df[sp,]
    
    ## sp_nr <- nrow(sp.df)
    ## span <- diff(range(sp.df$y))/sp_nr
    
    ## new_span <- span * scale
    old.sp.df <- sp.df
    sp.df$y <- df[node, "y"] + (sp.df$y - df[node, "y"]) * scale
    if (! vertical_only) {
        sp.df$x <- df[node, "x"] + (sp.df$x - df[node, "x"]) * scale
    }
    
    scale_diff.up <- max(sp.df$y) - max(old.sp.df$y)
    scale_diff.lw <- min(sp.df$y) - min(old.sp.df$y)
    
    ii <- df$y > max(old.sp.df$y)
    if (sum(ii) > 0) {
        df[ii, "y"] <- df[ii, "y"] + scale_diff.up
    }
    
    jj <- df$y < min(old.sp.df$y)
    if (sum(jj) > 0) {
        df[jj, "y"] <- df[jj, "y"] + scale_diff.lw
    }
    
    df[sp,] <- sp.df
    
    if (! "scale" %in% colnames(df)) {
        df$scale <- 1
    }
    df[sp, "scale"] <- df[sp, "scale"] * scale

    df <- reassign_y_from_node_to_root(df, node)
    
    ## re-calculate branch mid position
    df <- calculate_branch_mid(df)
    
    tree_view$data <- calculate_angle(df)
    tree_view
}


reassign_y_from_node_to_root <- function(df, node) {
    root <- which(df$node == df$parent)
    pp <- df[node, "parent"]
    while(any(pp != root)) {
        df[pp, "y"] <- mean(df[getChild.df(df, pp), "y"])
        pp <- df[pp, "parent"]
    }
    j <- getChild.df(df, pp)
    j <- j[j!=pp]
    df[pp, "y"] <- mean(df[j, "y"])
    return(df)
}
