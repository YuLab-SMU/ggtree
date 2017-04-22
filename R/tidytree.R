##' @importFrom ggplot2 fortify
##' @method fortify treedata
##' @export
fortify.treedata <- function(model, data, layout="rectangular", yscale="none",
                             ladderize=TRUE, right=FALSE, branch.length ="branch.length",
                             mrsd=NULL, ...) {

    model <- set_branch_length(model, branch.length)

    x <- reorder.phylo(get.tree(model), "postorder")
    if (is.null(x$edge.length) || branch.length == "none") {
        xpos <- getXcoord_no_length(x)
    } else {
        xpos <- getXcoord(x)
    }
    ypos <- getYcoord(x)
    N <- Nnode(x, internal.only=FALSE)
    xypos <- data.frame(node=1:N, x=xpos, y=ypos)

    df <- as.data.frame(model, branch.length="branch.length") # already set by set_branch_length
    idx <- is.na(df$parent)
    df$parent[idx] <- df$node[idx]
    rownames(df) <- df$node

    res <- merge(df, xypos, by='node', all.y=TRUE)

    ## add branch mid position
    res <- calculate_branch_mid(res)

    ## ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
    res <- calculate_angle(res)
    scaleY(as.phylo(model), res, yscale, layout, ...)
}

##' @method as.data.frame treedata
##' @export
## @importFrom treeio Nnode
## @importFrom treeio Ntip
as.data.frame.treedata <- function(x, row.names, optional, branch.length = "branch.length", ...) {
    tree <- set_branch_length(x, branch.length)

    ## res <- as.data.frame(tree@phylo)
    res <- as.data.frame_(tree@phylo)
    tree_anno <- get_tree_data(x)
    if (nrow(tree_anno) > 0) {
        res <- merge(res, tree_anno, by="node", all.x=TRUE)
    }
    return(res)
}

##@method as.data.frame phylo
##@export
as.data.frame_ <- function(x, row.names, optional, branch.length = "branch.length", ...) {
    phylo <- x
    ntip <- Ntip(phylo)
    N <- Nnode(phylo, internal.only=FALSE)

    tip.label <- phylo[["tip.label"]]
    res <- as.data.frame(phylo[["edge"]])
    colnames(res) <- c("parent", "node")
    if (!is.null(phylo$edge.length))
        res$branch.length <- phylo$edge.length

    label <- rep(NA, N)
    label[1:ntip] <- tip.label
    if ( !is.null(phylo$node.label) ) {
        label[(ntip+1):N] <- phylo$node.label
    }
    label.df <- data.frame(node=1:N, label=label)
    res <- merge(res, label.df, by='node', all.y=TRUE)
    isTip <- rep(FALSE, N)
    isTip[1:ntip] <- TRUE
    res$isTip <- isTip

    return(res)
}

get_tree_data <- function(tree_object) {
    if (is(tree_object, "codeml")) {
        tree_anno <- tree_object@mlc@dNdS
    } else if (is(tree_object, "codeml_mlc")) {
        tree_anno <- tree_object@dNdS
    } else if (is(tree_object, "beast")) {
        tree_anno <- tree_object@stats
    } else {
        tree_anno <- tree_object@data
    }

    if (has.extraInfo(tree_object)) {
        if (nrow(tree_anno) > 0) {
            tree_anno <- merge(tree_anno, tree_object@extraInfo, by="node")
        } else {
            return(tree_object@extraInfo)
        }
    }
    return(tree_anno)
}


##' convert tip or node label(s) to internal node number
##'
##'
##' @title nodeid
##' @param x tree object or graphic object return by ggtree
##' @param label tip or node label(s)
##' @return internal node number
##' @export
##' @author Guangchuang Yu
nodeid <- function(x, label) {
    if (is(x, "gg"))
        return(nodeid.gg(x, label))

    nodeid.tree(x, label)
}

nodeid.tree <- function(tree, label) {
    tr <- get.tree(tree)
    lab <- c(tr$tip.label, tr$node.label)
    match(label, lab)
}

nodeid.gg <- function(p, label) {
    p$data$node[match(label, p$data$label)]
}


reroot_node_mapping <- function(tree, tree2) {
    root <- getRoot(tree)

    node_map <- data.frame(from=1:getNodeNum(tree), to=NA, visited=FALSE)
    node_map[1:Ntip(tree), 2] <- match(tree$tip.label, tree2$tip.label)
    node_map[1:Ntip(tree), 3] <- TRUE

    node_map[root, 2] <- root
    node_map[root, 3] <- TRUE

    node <- rev(tree$edge[,2])
    for (k in node) {
        ip <- getParent(tree, k)
        if (node_map[ip, "visited"])
            next

        cc <- getChild(tree, ip)
        node2 <- node_map[cc,2]
        if (anyNA(node2)) {
            node <- c(node, k)
            next
        }

        to <- unique(sapply(node2, getParent, tr=tree2))
        to <- to[! to %in% node_map[,2]]
        node_map[ip, 2] <- to
        node_map[ip, 3] <- TRUE
    }
    node_map <- node_map[, -3]
    return(node_map)
}



##' @importFrom ape reorder.phylo
layout.unrooted <- function(tree, branch.length="branch.length", layout.method="equal_angle", ...) {

    df <- switch(layout.method,
                 equal_angle = layoutEqualAngle(tree, branch.length),
                 daylight = layoutDaylight(tree, branch.length)
                 )

    return(df)
}


##' 'Equal-angle layout algorithm for unrooted trees'
##'
##' @references
##' "Inferring Phylogenies" by Joseph Felsenstein.
##'
##' @title layoutEqualAngle
##' @param tree phylo object
##' @param branch.length set to 'none' for edge length of 1. Otherwise the phylogenetic tree edge length is used.
##' @return tree as data.frame with equal angle layout.
layoutEqualAngle <- function(tree, branch.length ){
    root <- getRoot(tree)
    ## Convert Phylo tree to data.frame.
    df <- as.data.frame.phylo_(tree)

    ## NOTE: Angles (start, end, angle) are in half-rotation units (radians/pi or degrees/180)

    ## create and assign NA to the following fields.
    df$x <- NA
    df$y <- NA
    df$start <- NA # Start angle of segment of subtree.
    df$end   <- NA # End angle of segment of subtree
    df$angle <- NA # Orthogonal angle to beta ... for labels??
    ## Initialize root node position and angles.
    df[root, "x"] <- 0
    df[root, "y"] <- 0
    df[root, "start"] <- 0 # 0-degrees
    df[root, "end"]   <- 2 # 360-degrees
    df[root, "angle"] <- 0 # Angle label.

    N <- getNodeNum(tree)

    ## Get number of tips for each node in tree.
    nb.sp <- sapply(1:N, function(i) length(get.offspring.tip(tree, i)))
    ## Get list of node id's.
    nodes <- getNodes_by_postorder(tree)

    for(curNode in nodes) {
        ## Get number of tips for current node.
        curNtip <- nb.sp[curNode]
        ## Get array of child node indexes of current node.
        children <- getChild(tree, curNode)

        ## Get "start" and "end" angles of a segment for current node in the data.frame.
        start <- df[curNode, "start"]
        end <- df[curNode, "end"]

        if (length(children) == 0) {
            ## is a tip
            next
        }

        for (i in seq_along(children)) {
            child <- children[i]
            ## Get the number of tips for child node.
            ntip.child <- nb.sp[child]

            ## Calculated in half radians.
            ## alpha: angle of segment for i-th child with ntips_ij tips.
            ## alpha = (left_angle - right_angle) * (ntips_ij)/(ntips_current)
            alpha <- (end - start) * ntip.child / curNtip
            ## beta = angle of line from parent node to i-th child.
            beta <- start + alpha / 2

            if (branch.length == "none") {
                length.child <- 1
            } else {
                length.child <- df[child, "length"]
            }

            ## update geometry of data.frame.
            ## Calculate (x,y) position of the i-th child node from current node.
            df[child, "x"] <- df[curNode, "x"] + cospi(beta) * length.child
            df[child, "y"] <- df[curNode, "y"] + sinpi(beta) * length.child
            ## Calculate orthogonal angle to beta.
            df[child, "angle"] <- -90 - 180 * beta * sign(beta - 1)
            ## Update the start and end angles of the childs segment.
            df[child, "start"] <- start
            df[child, "end"] <- start + alpha
            start <- start + alpha
        }

    }

    return(df)

}

##' Equal daylight layout method for unrooted trees.
##'
##' #' @title
##' @param tree phylo object
##' @param branch.length set to 'none' for edge length of 1. Otherwise the phylogenetic tree edge length is used.
##' @return tree as data.frame with equal angle layout.
##' @references
##' The following aglorithm aims to implement the vague description of the "Equal-daylight Algorithm"
##' in "Inferring Phylogenies" pp 582-584 by Joseph Felsenstein.
##'
##' ```
##' Leafs are subtrees with no children
##' Initialise tree using equal angle algorithm
##' tree_df = equal_angle(tree)
##'
##' nodes = get list of nodes in tree_df breadth-first
##' nodes = remove tip nodes.
##'
##' ```
layoutDaylight <- function( tree, branch.length ){

    ## How to set optimal
    MAX_COUNT <- 100
    MINIMUM_AVERAGE_ANGLE_CHANGE <- 0.01


    ## Initialize tree.
    tree_df <- layoutEqualAngle(tree, branch.length)

    ## nodes = get list of nodes in tree_df
    ## Get list of node id's.
    ## nodes <- getNodes_by_postorder(tree)
    ## nodes <- GetSubtree.df(tree_df, root)

    ## Get list of internal nodes
    ## nodes <- tree_df[tree_df$IsTip != TRUE]$nodes

    nodes <- getNodesBreadthFirst.df(tree_df)
    ## select only internal nodes
    internal_nodes <- tree_df[!tree_df$isTip,]$node
    ## Remove tips from nodes list, but keeping order.
    nodes <- intersect(nodes, internal_nodes)

    i <- 1
    ave_change <- 1.0
    while( i <= MAX_COUNT & ave_change > MINIMUM_AVERAGE_ANGLE_CHANGE ){
        message('Iteration: ', i)

        ## Reset max_change after iterating over tree.
        total_max <- 0.0

        ## for node in nodes {
        for( j in seq_along(nodes)){
            currentNode_id <- nodes[j]

            result <- applyLayoutDaylight(tree_df, currentNode_id)
            tree_df <- result$tree
            total_max <- total_max + result$max_change

        }

        ave_change <- total_max / length(nodes)

        message('Average angle change ', ave_change)

        i <- i + 1
    }

    return(tree_df)

}

##' Apply the daylight alorithm to adjust the spacing between the subtrees and tips of the
##' specified node.
##'
##' @title applyLayoutDaylight
##' @param df tree data.frame
##' @param node_id is id of the node from which daylight is measured to the other subtrees.
##' @return list with tree data.frame with updated layout using daylight algorithm and max_change angle.
##
##
## ```
## for node in nodes {
##   if node is a leaf {
##     next
##   }
##
##   subtrees = get subtrees of node
##
##   for i-th subtree in subtrees {
##     [end, start] = get left and right angles of tree from node id.
##     angle_list[i, 'left'] = end
##     angle_list[i, 'beta'] = start - end  # subtree arc angle
##     angle_list[i, 'index'] = i-th # index of subtree/leaf
##   }
##
##   sort angle_list by 'left' column in ascending order.
##
##   D = 360 - sum( angle_list['beta'] ) # total daylight angle
##   d = D / |subtrees| # equal daylight angle.
##
##   new_L = left angle of first subtree.
##
##   for n-th row in angle_list{
##     # Calculate angle to rotate subtree/leaf to create correct daylight angle.
##     new_left_angle = new_left_angle + d + angle_list[n, 'beta']
##     Calculate the difference between the old and new left angles.
##     adjust_angle = new_left_angle - angle_list[n, 'left']
##
##     index = angle_list['index']
##     rotate subtree[index] wrt n-th node by adjust_angle
##     }
##   }
## }
## ```
applyLayoutDaylight <- function(df, node_id ){

  max_change <- 0.0

  # Get lists of node ids for each subtree, including  rest of unrooted tree.
  subtrees <- getSubtreeUnrooted.df(df, node_id)
  angle_list <- data.frame(left=numeric(0), beta=numeric(0), subtree_id=integer(0) )

  # Return tree if only 2 or less subtrees to adjust.
  if(length(subtrees) <= 2){
    return( list(tree = df, max_change = max_change) )
  }

  # Find start and end angles for each subtree.
  #   subtrees = get subtrees of node
  #   for i-th subtree in subtrees {
  for (i in seq_along(subtrees) ) {
    subtree <- subtrees[[i]]
    # [end, start] = get start and end angles of tree.

    angles <- getTreeArcAngles(df, node_id, subtree)
    angle_list[ i, 'subtree_id'] <- i
    angle_list[ i, 'left'] <- angles['left']
    angle_list[ i, 'beta'] <- angles['left'] - angles['right'] # subtree arc angle
    # If subtree arc angle is -ve, then + 2 (360).
    if(angle_list[ i, 'beta'] < 0 ){
      angle_list[ i, 'beta'] <- angle_list[ i, 'beta'] + 2
    }
  }
  #   sort angle_list by 'left angle' column in ascending order.
  angle_list <- angle_list[with(angle_list, order(left)), ]
  #   D = 360 - sum( angle_list['beta'] ) # total day
  #   d = D / |subtrees| # equal daylight angle.
  total_daylight <- 2 - colSums(angle_list['beta'])
  d <- total_daylight / length(subtrees)

  # Initialise new left-angle as first subtree left-angle.
  new_left_angle <- angle_list[1, 'left']

  # Adjust angles of subtrees and tips connected to current node.
  # for n-th row in angle_list{
  # Skip the first subtree as it is not adjusted.
  for (i in 2:nrow(angle_list) ) {
    # Calculate angle to rotate subtree/leaf to create correct daylight angle.
    new_left_angle <- new_left_angle + d + angle_list[i, 'beta']
    # Calculate the difference between the old and new left angles.
    adjust_angle <- new_left_angle - angle_list[i, 'left']

    max_change <- max(max_change, abs(adjust_angle))
    #cat('Adjust angle:', abs(adjust_angle), ' Max change:', max_change ,'\n')

    # rotate subtree[index] wrt current node
    subtree_id <- angle_list[i, 'subtree_id']
    subtree_nodes <- subtrees[[subtree_id]]$subtree
    # update tree_df for all subtrees with rotated points.
    df <- rotateTreePoints.df(df, node_id, subtree_nodes, adjust_angle)
  }

  return( list(tree = df, max_change = max_change) )

}


##' Find the right (clockwise rotation, angle from +ve x-axis to furthest subtree nodes) and
##' left (anti-clockwise angle from +ve x-axis to subtree)
##'
##' @title getTreeArcAngles
##' @param df tree data.frame
##' @param origin_id node id from which to calculate left and right hand angles of subtree.
##' @param subtree named list of root id of subtree and list of node ids for given subtree.
##' @return named list with right and left angles in range [0,2] i.e 1 = 180 degrees, 1.5 = 270 degrees.
getTreeArcAngles <- function(df, origin_id, subtree) {
  # Initialise variables
  theta_child <- 0.0
  subtree_root_id <- subtree$node
  subtree_node_ids <- subtree$subtree

  # Initialise angle from origin node to parent node.
  # If subtree_root_id is child of origin_id
  if( any(subtree_root_id == getChild.df(df, origin_id)) ){
    theta_parent <- getNodeAngle.df(df, origin_id, subtree_root_id)
  }else{
    # get the real root of df tree to initialise left and right angles.
    theta_parent <- getNodeAngle.df(df, origin_id, getRoot.df(df))
  }

  # create vector with named columns
  # left-hand and right-hand angles between origin node and the extremities of the tree nodes.
  arc <- c('left' = theta_parent, 'right' = theta_parent)

  # Subtree has to have 1 or more nodes to compare.
  if (length(subtree_node_ids) == 0 ){
    return(0)
  }


  # Remove tips from nodes list, but keeping order.
  # internal_nodes <- df[!df$isTip,]$node
  # subtree_node_ids <- intersect(subtree_node_ids, internal_nodes)


  # Calculate the angle from the origin node to each child node.
  # Moving from parent to children in depth-first traversal.
  for( i in seq_along(subtree_node_ids) ){
    parent_id <- subtree_node_ids[i]
    # Get angle from origin node to parent node.
    # Skip if parent_id is a tip.
    if(isTip.df(df, parent_id) ){ next }

    theta_parent <- getNodeAngle.df(df, origin_id, parent_id)

    children_ids <- getChild.df(df, parent_id)

    for( j in seq_along(children_ids)){
      #delta_x <- df[subtree_node_id, 'x'] - df[origin_id, 'x']
      #delta_y <- df[subtree_node_id, 'y'] - df[origin_id, 'y']
      #angles[i] <- atan2(delta_y, delta_x) / pi
      child_id <- children_ids[j]
      # Skip if child is parent node of subtree.
      if( child_id == origin_id ){
        next
      }

      theta_child <- getNodeAngle.df(df, origin_id, child_id)

      # Skip if child node is already inside arc.
      # if left < right angle (arc crosses 180/-180 quadrant) and child node is not inside arc of tree.
      # OR if left > right angle (arc crosses 0/360 quadrant) and child node is inside gap
      if ( (arc['left'] < arc['right'] & !( theta_child > arc['left'] & theta_child < arc['right'])) |
        (arc['left'] > arc['right'] & ( theta_child < arc['left'] & theta_child > arc['right'])) ){
        # child node inside arc.
        next
      }


      delta <- theta_child - theta_parent
      delta_adj <- delta
      # Correct the delta if parent and child angles cross the 180/-180 half of circle.
      # If delta > 180
      if( delta > 1){ # Edge between parent and child cross upper and lower quadrants of cirlce on 180/-180 side.
        delta_adj <- delta - 2 # delta' = delta - 360
      # If delta < -180
      }else if( delta < -1){ # Edge between parent and child cross upper and lower quadrants of cirlce
        delta_adj <- delta + 2 # delta' = delta - 360
      }

      theta_child_adj <- theta_child

      # If angle change from parent to node is positive (anti-clockwise), check left angle
      if(delta_adj > 0){
        # If child/parent edges cross the -180/180 quadrant (angle between them is > 180),
        # check if right angle and child angle are different signs and adjust if needed.
        if( abs(delta) > 1){
          if( arc['left'] > 0 & theta_child < 0){
            theta_child_adj <- theta_child + 2
          }else if (arc['left'] < 0 & theta_child > 0){
            theta_child_adj <- theta_child - 2
          }
        }

          # check if left angle of arc is less than angle of child. Update if true.
        if( arc['left'] < theta_child_adj ){
          arc['left'] <- theta_child
        }
      # If angle change from parent to node is negative (clockwise), check right angle
      }else if(delta_adj < 0){
        # If child/parent edges cross the -180/180 quadrant (angle between them is > 180),
        # check if right angle and child angle are different signs and adjust if needed.
        if( abs(delta) > 1){
          # Else change in angle from parent to child is negative, then adjust child angle if right angle is a different sign.
          if( arc['right'] > 0 & theta_child < 0){
            theta_child_adj <- theta_child + 2
          }else if (arc['right'] < 0 & theta_child > 0){
            theta_child_adj <- theta_child - 2
          }
        }
        # check if right angle of arc is greater than angle of child. Update if true.
        if( arc['right'] > theta_child_adj  ){
          arc['right'] <- theta_child
        }

      }
    }

  }
  # Convert arc angles of [1, -1] to [2,0] domain.
  arc[arc<0] <- arc[arc<0] + 2
  return(arc)

}

##' Rotate the points in a tree data.frame around a pivot node by the angle specified.
##'
##' @title rotateTreePoints.data.fram
##' @param df tree data.frame
##' @param pivot_node is the id of the pivot node.
##' @param nodes list of node numbers that are to be rotated by angle around the pivot_node
##' @param angle in range [0,2], ie degrees/180, radians/pi
##' @return updated tree data.frame with points rotated by angle
rotateTreePoints.df <- function(df, pivot_node, nodes, angle){
  # Rotate nodes around pivot_node.
  # x' = cos(angle)*delta_x - sin(angle)*delta_y + delta_x
  # y' = sin(angle)*delta_x + cos(angle)*delta_y + delta_y

  cospitheta <- cospi(angle)
  sinpitheta <- sinpi(angle)
  for(node in nodes){
    # Update (x,y) of node
    delta_x <- df[node, 'x'] - df[pivot_node, 'x']
    delta_y <- df[node, 'y'] - df[pivot_node, 'y']
    df[node, 'x'] <- cospitheta * delta_x - sinpitheta * delta_y + df[pivot_node, 'x']
    df[node, 'y'] <- sinpitheta * delta_x + cospitheta * delta_y + df[pivot_node, 'y']

    # Update label angle if not root node.
    # get parent
    parent_id <- getParent.df(df, node)
    # if 'node' is not root, then update label angle.
    if( parent_id != 0){
      theta_parent_child <- getNodeAngle.df(df, parent_id, node)
      if(!is.na(theta_parent_child)){
        # Update label angle
        df[node, 'angle'] <- -90 - 180 * theta_parent_child * sign(theta_parent_child - 1)
      }
    }

  }
  return(df)
}

##' Get the angle between the two nodes specified.
##'
##' @title getNodeAngle.df
##' @param df tree data.frame
##' @param origin_node_id origin node id number
##' @param node_id end node id number
##' @return angle in range [-1, 1], i.e. degrees/180, radians/pi
getNodeAngle.df <- function(df, origin_node_id, node_id){
  if(origin_node_id != node_id){
    delta_x <- df[node_id, 'x'] - df[origin_node_id, 'x']
    delta_y <- df[node_id, 'y'] - df[origin_node_id, 'y']
    angle <- atan2(delta_y, delta_x) / pi
    return( angle )
  }else{
    return(NA)
  }
}



##' Get all children of node from tree, including start_node.
##'
##' @title getSubtree
##' @param tree ape phylo tree object
##' @param node is the tree node id from which the tree is derived.
##' @return list of all child node id's from starting node.
getSubtree <- function(tree, node){

  subtree <- c(node)
  i <- 1
  while( i <= length(subtree)){
    subtree <- c(subtree, getChild(tree, subtree[i]))
    # remove any '0' root nodes
    subtree <- subtree[subtree != 0]
    i <- i + 1
  }
  return(subtree)
}

##' Get all children of node from df tree using breath-first.
##'
##' @title GetSubtree.df
##' @param df tree data.frame
##' @param node id of starting node.
##' @return list of all child node id's from starting node.
GetSubtree.df <- function(df, node){
  subtree <- c(node)
  i <- 1
  while( i <= length(subtree)){
    subtree <- c(subtree, getChild.df(df, subtree[i]))
    # remove any '0' root nodes
    subtree <- subtree[subtree != 0]
    i <- i + 1
  }
  return(subtree)
}

##' Get all subtrees of specified node. This includes all ancestors and relatives of node and
##' return named list of subtrees.
##'
##' @title GetSubtreeUnrooted
##' @param tree ape phylo tree object
##' @param node is the tree node id from which the subtrees are derived.
##' @return named list of subtrees with the root id of subtree and list of node id's making up subtree.
GetSubtreeUnrooted <- function(tree, node){
  # if node leaf, return nothing.
  if( isTip(tree, node) ){
    # return NA
    return(NA)
  }

  subtrees <- list()

  # get subtree for each child node.
  children_ids <- getChild(tree, node)

  remaining_nodes <- getNodes_by_postorder(tree)
  # Remove current node from remaining_nodes list.
  remaining_nodes <- setdiff(remaining_nodes, node)


  for( child in children_ids ){
    # Append subtree nodes to list if not 0 (root).
    subtree <- getSubtree(tree, child)
    subtrees[[length(subtrees)+1]] <- list( node = child, subtree = subtree)
    # remove subtree nodes from remaining nodes.
    remaining_nodes <- setdiff(remaining_nodes, as.integer(unlist(subtrees[[length(subtrees)]]['subtree']) ))
  }

  # The remaining nodes that are not found in the child subtrees are the remaining subtree nodes.
  # ie, parent node and all other nodes. We don't care how they are connect, just their ids.
  parent_id <- getParent(tree, node)
  # If node is not root, add remainder of tree nodes as subtree.
  if( parent_id != 0 & length(remaining_nodes) >= 1){
    subtrees[[length(subtrees)+1]] <- list( node = parent_id, subtree = remaining_nodes)
  }

  return(subtrees)
}


##' Get all subtrees of node, as well as remaining branches of parent (ie, rest of tree structure as subtree)
##' return named list of subtrees with list name as starting node id.
##' @title GetSubtreeUnrooted
##' @param df tree data.frame
##' @param node is the tree node id from which the subtrees are derived.
##' @return named list of subtrees with the root id of subtree and list of node id's making up subtree.
getSubtreeUnrooted.df <- function(df, node){
  # if node leaf, return nothing.
  if( isTip.df(df, node) ){
    return(NA)
  }

  subtrees <- list()

  # get subtree for each child node.
  children_ids <- getChild.df(df, node)

  # remaining_nodes <- getNodes_by_postorder(tree)
  remaining_nodes <- df$node

  # Remove current node from remaining_nodes list.
  remaining_nodes <- setdiff(remaining_nodes, node)

  for( child in children_ids ){
    subtree <- GetSubtree.df(df, child)
    # Append subtree nodes to list if more than 1 node in subtree (i.e. not a tip)
    #if(length(subtree) >= 2){
      subtrees[[length(subtrees)+1]] <- list( node = child, subtree = subtree)
      # remove subtree nodes from remaining nodes.
      remaining_nodes <- setdiff(remaining_nodes, as.integer(unlist(subtrees[[length(subtrees)]]['subtree']) ))
    #}else{
      # remove remaining nodes
    #  remaining_nodes <- setdiff(remaining_nodes, subtree)
    #}
  }

  # The remaining nodes that are not found in the child subtrees are the remaining subtree nodes.
  # ie, parent node and all other nodes. We don't care how they are connected, just their id.
  parent_id <- getParent.df(df, node)
  # If node is not root.
  if( parent_id != 0 & length(remaining_nodes) >= 1){
    subtrees[[length(subtrees)+1]] <- list( node = parent_id, subtree = remaining_nodes)
  }

  return(subtrees)
}


getRoot.df <- function(df, node){
  root <- which(is.na(df$parent))
  return(root)
}

##' Get parent node id of child node.
##'
##' @title getParent.df
##' @param df tree data.frame
##' @param node is the node id of child in tree.
##' @return integer node id of parent
getParent.df <- function(df, node) {
    i <- which(df$node == node)
    parent_id <- df$parent[i]
    if (parent_id == node | is.na(parent_id)) {
        ## root node
        return(0)
    }
    return(parent_id)
}

getAncestor.df <- function(df, node) {
    anc <- getParent.df(df, node)
    anc <- anc[anc != 0]
    if (length(anc) == 0) {
        # stop("selected node is root...")
      return(0)
    }
    i <- 1
    while(i<= length(anc)) {
        anc <- c(anc, getParent.df(df, anc[i]))
        anc <- anc[anc != 0]
        i <- i+1
    }
    return(anc)
}


##' Get list of child node id numbers of parent node
##'
##' @title getChild.df
##' @param df tree data.frame
##' @param node is the node id of child in tree.
##' @return list of child node ids of parent
getChild.df <- function(df, node) {
    i <- which(df$parent == node)
    if (length(i) == 0) {
        return(0)
    }
    res <- df[i, "node"]
    res <- res[res != node] ## node may root
    return(res)
}

get.offspring.df <- function(df, node) {
    sp <- getChild.df(df, node)
    sp <- sp[sp != 0]
    if (length(sp) == 0) {
        #stop("input node is a tip...")
      return(0)
    }

    i <- 1
    while(i <= length(sp)) {
        sp <- c(sp, getChild.df(df, sp[i]))
        sp <- sp[sp != 0]
        i <- i + 1
    }
    return(sp)
}


##' extract offspring tips
##'
##'
##' @title get.offspring.tip
##' @param tr tree
##' @param node node
##' @return tip label
##' @author ygc
##' @importFrom ape extract.clade
##' @export
get.offspring.tip <- function(tr, node) {
    if ( ! node %in% tr$edge[,1]) {
        ## return itself
        return(tr$tip.label[node])
    }
    clade <- extract.clade(tr, node)
    clade$tip.label
}


## ##' calculate total number of nodes
## ##'
## ##'
## ##' @title getNodeNum
## ##' @param tr phylo object
## ##' @return number
## ##' @author Guangchuang Yu
## ##' @export
## getNodeNum <- function(tr) {
##     Ntip <- length(tr[["tip.label"]])
##     Nnode <- tr[["Nnode"]]
##     ## total nodes
##     N <- Ntip + Nnode
##     return(N)
## }
getParent <- function(tr, node) {
    if ( node == getRoot(tr) )
        return(0)
    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
    res <- parent[child == node]
    if (length(res) == 0) {
        stop("cannot found parent node...")
    }
    if (length(res) > 1) {
        stop("multiple parent found...")
    }
    return(res)
}

getChild <- function(tr, node) {
    # Get edge matrix from phylo object.
    edge <- tr[["edge"]]
    # Select all rows that match "node".
    res <- edge[edge[,1] == node, 2]
    ## if (length(res) == 0) {
    ##     ## is a tip
    ##     return(NA)
    ## }
    return(res)
}

getSibling <- function(tr, node) {
    root <- getRoot(tr)
    if (node == root) {
        return(NA)
    }

    parent <- getParent(tr, node)
    child <- getChild(tr, parent)
    sib <- child[child != node]
    return(sib)
}


getAncestor <- function(tr, node) {
    root <- getRoot(tr)
    if (node == root) {
        return(NA)
    }
    parent <- getParent(tr, node)
    res <- parent
    while(parent != root) {
        parent <- getParent(tr, parent)
        res <- c(res, parent)
    }
    return(res)
}

isRoot <- function(tr, node) {
    getRoot(tr) == node
}

isTip <- function(tr, node) {
  children_ids <- getChild(tr, node)
  length(children_ids) == 0
}

isTip.df <- function(df, node) {
  return(df[node, 'isTip'])
}


getNodeName <- function(tr) {
    if (is.null(tr$node.label)) {
        n <- length(tr$tip.label)
        nl <- (n + 1):(2 * n - 2)
        nl <- as.character(nl)
    }
    else {
        nl <- tr$node.label
    }
    nodeName <- c(tr$tip.label, nl)
    return(nodeName)
}

## ##' get the root number
## ##'
## ##'
## ##' @title getRoot
## ##' @param tr phylo object
## ##' @return root number
## ##' @export
## ##' @author Guangchuang Yu
## getRoot <- function(tr) {
##     edge <- tr[["edge"]]
##     ## 1st col is parent,
##     ## 2nd col is child,
##     if (!is.null(attr(tr, "order")) && attr(tr, "order") == "postorder")
##         return(edge[nrow(edge), 1])

##     parent <- unique(edge[,1])
##     child <- unique(edge[,2])
##     ## the node that has no parent should be the root
##     root <- parent[ ! parent %in% child ]
##     if (length(root) > 1) {
##         stop("multiple roots founded...")
##     }
##     return(root)
## }
get.trunk <- function(tr) {
    root <- getRoot(tr)
    path_length <- sapply(1:(root-1), function(x) get.path_length(tr, root, x))
    i <- which.max(path_length)
    return(get.path(tr, root, i))
}

##' path from start node to end node
##'
##'
##' @title get.path
##' @param phylo phylo object
##' @param from start node
##' @param to end node
##' @return node vectot
##' @export
##' @author Guangchuang Yu
get.path <- function(phylo, from, to) {
    anc_from <- getAncestor(phylo, from)
    anc_from <- c(from, anc_from)
    anc_to <- getAncestor(phylo, to)
    anc_to <- c(to, anc_to)
    mrca <- intersect(anc_from, anc_to)[1]

    i <- which(anc_from == mrca)
    j <- which(anc_to == mrca)

    path <- c(anc_from[1:i], rev(anc_to[1:(j-1)]))
    return(path)
}

get.path_length <- function(phylo, from, to, weight=NULL) {
    path <- get.path(phylo, from, to)
    if (is.null(weight)) {
        return(length(path)-1)
    }

    df <- fortify(phylo)
    if ( ! (weight %in% colnames(df))) {
        stop("weight should be one of numerical attributes of the tree...")
    }

    res <- 0

    get_edge_index <- function(df, from, to) {
        which((df[,1] == from | df[,2] == from) &
                  (df[,1] == to | df[,2] == to))
    }

    for(i in 1:(length(path)-1)) {
        ee <- get_edge_index(df, path[i], path[i+1])
        res <- res + df[ee, weight]
    }

    return(res)
}

getNodes_by_postorder <- function(tree) {
  tree <- reorder.phylo(tree, "postorder")
    unique(rev(as.vector(t(tree$edge[,c(2,1)]))))
}

#getNodes_by_postorder.df <- function(df) {
    #tree <- reorder.phylo(tree, "postorder")
    #unique(rev(as.vector(t(tree$edge[,c(2,1)]))))
#}

##' Get the nodes of tree from root in breadth-first order.
##'
##' @title getNodesBreadthFirst.df
##' @param df tree data.frame
##' @return list of node id's in breadth-first order.
getNodesBreadthFirst.df <- function(df){

  root <- getRoot.df(df)
  if(isTip.df(df, root)){
    return(root)
  }

  tree_size <- nrow(df)
  # initialise list of nodes
  res <- root

  i <- 1
  while(length(res) < tree_size){
    parent <- res[i]
    i <- i + 1

    # Skip if parent is a tip.
    if(isTip.df(df, parent)){
      next
    }

    # get children of current parent.
    children <- getChild.df(df,parent)

    # add children to result
    res <- c(res, children)

  }

  return(res)

}


getXcoord2 <- function(x, root, parent, child, len, start=0, rev=FALSE) {
    x[root] <- start
    x[-root] <- NA  ## only root is set to start, by default 0

    currentNode <- root
    direction <- 1
    if (rev == TRUE) {
        direction <- -1
    }
    while(anyNA(x)) {
        idx <- which(parent %in% currentNode)
        newNode <- child[idx]
        x[newNode] <- x[parent[idx]]+len[idx] * direction
        currentNode <- newNode
    }

    return(x)
}

getXcoord_no_length <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    ntip <- Ntip(tr)
    currentNode <- 1:ntip
    x[-currentNode] <- NA

    cl <- split(child, parent)
    child_list <- list()
    child_list[as.numeric(names(cl))] <- cl

    while(anyNA(x)) {
        idx <- match(currentNode, child)
        pNode <- parent[idx]
        ## child number table
        p1 <- table(parent[parent %in% pNode])
        p2 <- table(pNode)
        np <- names(p2)
        i <- p1[np] == p2
        newNode <- as.numeric(np[i])

        exclude <- rep(NA, max(child))
        for (j in newNode) {
            x[j] <- min(x[child_list[[j]]]) - 1
            exclude[child_list[[j]]] <- child_list[[j]]
        }
        exclude <- exclude[!is.na(exclude)]

        ## currentNode %<>% `[`(!(. %in% exclude))
        ## currentNode %<>% c(., newNode) %>% unique
        currentNode <- currentNode[!currentNode %in% exclude]
        currentNode <- unique(c(currentNode, newNode))

    }
    x <- x - min(x)
    return(x)
}


getXcoord <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    x <- getXcoord2(x, root, parent, child, len)
    return(x)
}

getXYcoord_slanted <- function(tr) {

    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    N <- getNodeNum(tr)
    len <- tr$edge.length
    y <- getYcoord(tr, step=min(len)/2)

    len <- sqrt(len^2 - (y[parent]-y[child])^2)
    x <- numeric(N)
    x <- getXcoord2(x, root, parent, child, len)
    res <- data.frame(x=x, y=y)
    return(res)
}


## @importFrom magrittr %>%
##' @importFrom magrittr equals
getYcoord <- function(tr, step=1) {
    Ntip <- length(tr[["tip.label"]])
    N <- getNodeNum(tr)

    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]

    cl <- split(child, parent)
    child_list <- list()
    child_list[as.numeric(names(cl))] <- cl

    y <- numeric(N)
    tip.idx <- child[child <= Ntip]
    y[tip.idx] <- 1:Ntip * step
    y[-tip.idx] <- NA

    currentNode <- 1:Ntip
    while(anyNA(y)) {
        pNode <- unique(parent[child %in% currentNode])
        ## piping of magrittr is slower than nested function call.
        ## pipeR is fastest, may consider to use pipeR
        ##
        ## child %in% currentNode %>% which %>% parent[.] %>% unique
        ## idx <- sapply(pNode, function(i) all(child[parent == i] %in% currentNode))
        idx <- sapply(pNode, function(i) all(child_list[[i]] %in% currentNode))
        newNode <- pNode[idx]

        y[newNode] <- sapply(newNode, function(i) {
            mean(y[child_list[[i]]], na.rm=TRUE)
            ##child[parent == i] %>% y[.] %>% mean(na.rm=TRUE)
        })

        currentNode <- c(currentNode[!currentNode %in% unlist(child_list[newNode])], newNode)
        ## currentNode <- c(currentNode[!currentNode %in% child[parent %in% newNode]], newNode)
        ## parent %in% newNode %>% child[.] %>%
        ##     `%in%`(currentNode, .) %>% `!` %>%
        ##         currentNode[.] %>% c(., newNode)
    }

    return(y)
}


getYcoord_scale <- function(tr, df, yscale) {

    N <- getNodeNum(tr)
    y <- numeric(N)

    root <- getRoot(tr)
    y[root] <- 0
    y[-root] <- NA

    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]

    currentNodes <- root
    while(anyNA(y)) {
        newNodes <- c()
        for (currentNode in currentNodes) {
            idx <- which(parent %in% currentNode)
            newNode <- child[idx]
            direction <- -1
            for (i in seq_along(newNode)) {
                y[newNode[i]] <- y[currentNode] + df[newNode[i], yscale] * direction
                direction <- -1 * direction
            }
            newNodes <- c(newNodes, newNode)
        }
        currentNodes <- unique(newNodes)
    }
    if (min(y) < 0) {
        y <- y + abs(min(y))
    }
    return(y)
}

getYcoord_scale2 <- function(tr, df, yscale) {
    root <- getRoot(tr)

    pathLength <- sapply(1:length(tr$tip.label), function(i) {
        get.path_length(tr, i, root, yscale)
    })

    ordered_tip <- order(pathLength, decreasing = TRUE)
    ii <- 1
    ntip <- length(ordered_tip)
    while(ii < ntip) {
        sib <- getSibling(tr, ordered_tip[ii])
        if (length(sib) == 0) {
            ii <- ii + 1
            next
        }
        jj <- which(ordered_tip %in% sib)
        if (length(jj) == 0) {
            ii <- ii + 1
            next
        }
        sib <- ordered_tip[jj]
        ordered_tip <- ordered_tip[-jj]
        nn <- length(sib)
        if (ii < length(ordered_tip)) {
            ordered_tip <- c(ordered_tip[1:ii],sib, ordered_tip[(ii+1):length(ordered_tip)])
        } else {
            ordered_tip <- c(ordered_tip[1:ii],sib)
        }

        ii <- ii + nn + 1
    }


    long_branch <- getAncestor(tr, ordered_tip[1]) %>% rev
    long_branch <- c(long_branch, ordered_tip[1])

    N <- getNodeNum(tr)
    y <- numeric(N)

    y[root] <- 0
    y[-root] <- NA

    ## yy <- df[, yscale]
    ## yy[is.na(yy)] <- 0

    for (i in 2:length(long_branch)) {
        y[long_branch[i]] <- y[long_branch[i-1]] + df[long_branch[i], yscale]
    }

    parent <- df[, "parent"]
    child <- df[, "node"]

    currentNodes <- root
    while(anyNA(y)) {
        newNodes <- c()
        for (currentNode in currentNodes) {
            idx <- which(parent %in% currentNode)
            newNode <- child[idx]
            newNode <- c(newNode[! newNode %in% ordered_tip],
                         rev(ordered_tip[ordered_tip %in% newNode]))
            direction <- -1
            for (i in seq_along(newNode)) {
                if (is.na(y[newNode[i]])) {
                    y[newNode[i]] <- y[currentNode] + df[newNode[i], yscale] * direction
                    direction <- -1 * direction
                }
            }
            newNodes <- c(newNodes, newNode)
        }
        currentNodes <- unique(newNodes)
    }
    if (min(y) < 0) {
        y <- y + abs(min(y))
    }
    return(y)
}

getYcoord_scale_numeric <- function(tr, df, yscale, ...) {
    df <- .assign_parent_status(tr, df, yscale)
    df <- .assign_child_status(tr, df, yscale)

    y <- df[, yscale]

    if (anyNA(y)) {
        warning("NA found in y scale mapping, all were setting to 0")
        y[is.na(y)] <- 0
    }

    return(y)
}

.assign_parent_status <- function(tr, df, variable) {
    yy <- df[, variable]
    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
        tree <- get.tree(tr)
        nodes <- getNodes_by_postorder(tree)
        for (curNode in nodes) {
            children <- getChild(tree, curNode)
            if (length(children) == 0) {
                next
            }
            idx <- which(is.na(yy[children]))
            if (length(idx) > 0) {
                yy[children[idx]] <- yy[curNode]
            }
        }
    }
    df[, variable] <- yy
    return(df)
}

.assign_child_status <- function(tr, df, variable, yscale_mapping=NULL) {
    yy <- df[, variable]
    if (!is.null(yscale_mapping)) {
        yy <- yscale_mapping[yy]
    }

    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
        tree <- get.tree(tr)
        nodes <- rev(getNodes_by_postorder(tree))
        for (curNode in nodes) {
            parent <- getParent(tree, curNode)
            if (parent == 0) { ## already reach root
                next
            }
            idx <- which(is.na(yy[parent]))
            if (length(idx) > 0) {
                child <- getChild(tree, parent)
                yy[parent[idx]] <- mean(yy[child], na.rm=TRUE)
            }
        }
    }
    df[, variable] <- yy
    return(df)
}


getYcoord_scale_category <- function(tr, df, yscale, yscale_mapping=NULL, ...) {
    if (is.null(yscale_mapping)) {
        stop("yscale is category variable, user should provide yscale_mapping,
             which is a named vector, to convert yscale to numberical values...")
    }
    if (! is(yscale_mapping, "numeric") ||
        is.null(names(yscale_mapping))) {
        stop("yscale_mapping should be a named numeric vector...")
    }

    if (yscale == "label") {
        yy <- df[, yscale]
        ii <- which(is.na(yy))
        if (length(ii)) {
            df[ii, yscale] <- df[ii, "node"]
        }
    }

    ## assign to parent status is more prefer...
    df <- .assign_parent_status(tr, df, yscale)
    df <- .assign_child_status(tr, df, yscale, yscale_mapping)

    y <- df[, yscale]

    if (anyNA(y)) {
        warning("NA found in y scale mapping, all were setting to 0")
        y[is.na(y)] <- 0
    }
    return(y)
}


add_angle_slanted <- function(res) {
    dy <- (res[, "y"] - res[res$parent, "y"]) / diff(range(res[, "y"]))
    dx <- (res[, "x"] - res[res$parent, "x"]) / diff(range(res[, "x"]))
    theta <- atan(dy/dx)
    theta[is.na(theta)] <- 0 ## root node
    res$angle <- theta/pi * 180

    branch.y <- (res[res$parent, "y"] + res[, "y"])/2
    idx <- is.na(branch.y)
    branch.y[idx] <- res[idx, "y"]
    res[, "branch.y"] <- branch.y
    return(res)
}

calculate_branch_mid <- function(res) {
    res$branch <- (res[res$parent, "x"] + res[, "x"])/2
    if (!is.null(res$length)) {
        res$length[is.na(res$length)] <- 0
    }
    res$branch[is.na(res$branch)] <- 0
    return(res)
}


set_branch_length <- function(tree_object, branch.length) {
    if (branch.length == "branch.length") {
        return(tree_object)
    } else if (branch.length == "none") {
        tree_object@phylo$edge.length <- NULL
        return(tree_object)
    }

    if (is(tree_object, "phylo")) {
        return(tree_object)
    }

    tree_anno <- get_tree_data(tree_object)

    phylo <- get.tree(tree_object)

    cn <- colnames(tree_anno)
    cn <- cn[!cn %in% c('node', 'parent')]

    length <- match.arg(branch.length, cn)

    if (all(is.na(as.numeric(tree_anno[, length])))) {
        stop("branch.length should be numerical attributes...")
    }

    edge <- as.data.frame(phylo$edge)
    colnames(edge) <- c("parent", "node")

    dd <- merge(edge, tree_anno,
                by  = "node",
                all.x = TRUE)
    dd <- dd[match(edge$node, dd$node),]
    len <- unlist(dd[, length])
    len <- as.numeric(len)
    len[is.na(len)] <- 0

    phylo$edge.length <- len

    tree_object@phylo <- phylo
    return(tree_object)
}

## set_branch_length <- function(tree_object, branch.length) {
##     if (is(tree_object, "phylo4d")) {
##         phylo <- as.phylo.phylo4(tree_object)
##         d <- tree_object@data
##         tree_anno <- data.frame(node=rownames(d), d)
##     } else {
##         phylo <- get.tree(tree_object)
##     }

##     if (branch.length %in%  c("branch.length", "none")) {
##         return(phylo)
##     }

##     ## if (is(tree_object, "codeml")) {
##     ##     tree_anno <- tree_object@mlc@dNdS
##     ## } else

##     if (is(tree_object, "codeml_mlc")) {
##         tree_anno <- tree_object@dNdS
##     } else if (is(tree_object, "beast")) {
##         tree_anno <- tree_object@stats
##     }

##     if (has.extraInfo(tree_object)) {
##         tree_anno <- merge(tree_anno, tree_object@extraInfo, by.x="node", by.y="node")
##     }
##     cn <- colnames(tree_anno)
##     cn <- cn[!cn %in% c('node', 'parent')]

##     length <- match.arg(branch.length, cn)

##     if (all(is.na(as.numeric(tree_anno[, length])))) {
##         stop("branch.length should be numerical attributes...")
##     }

##     edge <- as.data.frame(phylo$edge)
##     colnames(edge) <- c("parent", "node")

##     dd <- merge(edge, tree_anno,
##                 by.x  = "node",
##                 by.y  = "node",
##                 all.x = TRUE)
##     dd <- dd[match(edge$node, dd$node),]
##     len <- unlist(dd[, length])
##     len <- as.numeric(len)
##     len[is.na(len)] <- 0

##     phylo$edge.length <- len

##     return(phylo)
## }
re_assign_ycoord_df <- function(df, currentNode) {
    while(anyNA(df$y)) {
        pNode <- with(df, parent[match(currentNode, node)]) %>% unique
        idx <- sapply(pNode, function(i) with(df, all(node[parent == i & parent != node] %in% currentNode)))
        newNode <- pNode[idx]
        ## newNode <- newNode[is.na(df[match(newNode, df$node), "y"])]

        df[match(newNode, df$node), "y"] <- sapply(newNode, function(i) {
            with(df, mean(y[parent == i], na.rm = TRUE))
        })
        traced_node <- as.vector(sapply(newNode, function(i) with(df, node[parent == i])))
        currentNode <- c(currentNode[! currentNode %in% traced_node], newNode)
    }
    return(df)
}


## ##' test whether input object is produced by ggtree function
## ##'
## ##'
## ##' @title is.ggtree
## ##' @param x object
## ##' @return TRUE or FALSE
## ##' @export
## ##' @author guangchuang yu
## is.ggtree <- function(x) inherits(x, 'ggtree')


calculate_angle <- function(data) {
    data$angle <- 360/(diff(range(data$y)) + 1) * data$y
    return(data)
}
