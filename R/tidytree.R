








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
    df$angle <- NA # Orthogonal angle to beta for tip labels.
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
            ## Calculate orthogonal angle to beta for tip label.
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
    MAX_COUNT <- 5
    MINIMUM_AVERAGE_ANGLE_CHANGE <- 0.05


    ## Initialize tree.
    tree_df <- layoutEqualAngle(tree, branch.length)

    ## nodes = get list of nodes in tree_df
    ## Get list of node id's.
    ## nodes <- getNodes_by_postorder(tree)
    ## nodes <- getSubtree.df(tree_df, root)

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
        # Calculate the running average of angle changes.
        ave_change <- total_max / length(nodes) * length(i)

        cat('Average angle change [',i,']', ave_change,'\n')

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
applyLayoutDaylight <- function(df, node_id){

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
##' left (anti-clockwise angle from +ve x-axis to subtree) Returning arc angle in [0, 2] (0 to 360) domain.
##'
##' @title getTreeArcAngles
##' @param df tree data.frame
##' @param origin_id node id from which to calculate left and right hand angles of subtree.
##' @param subtree named list of root id of subtree (node) and list of node ids for given subtree (subtree).
##' @return named list with right and left angles in range [0,2] i.e 1 = 180 degrees, 1.5 = 270 degrees.
getTreeArcAngles <- function(df, origin_id, subtree) {
  # Initialise variables
  theta_child <- 0.0
  subtree_root_id <- subtree$node
  subtree_node_ids <- subtree$subtree

  # Initialise angle from origin node to parent node.
  # If subtree_root_id is child of origin_id
  if( any(subtree_root_id == getChild.df(df, origin_id)) ){
    # get angle from original node to parent of subtree.
    theta_left <- getNodeAngle.df(df, origin_id, subtree_root_id)
    theta_right <- theta_left
  }else if( subtree_root_id == origin_id){
    # Special case.
    # get angle from parent of subtree to children
    children_ids <- getChild.df(df, subtree_root_id)

    if(length(children_ids) == 2){
      # get angles from parent to it's two children.
      theta1 <- getNodeAngle.df(df, origin_id, children_ids[1])
      theta2 <- getNodeAngle.df(df, origin_id, children_ids[2])

      delta <- theta1 - theta2


      # correct delta for points crossing 180/-180 quadrant.
      if(delta > 1){
        delta_adj = delta - 2
      }else if(delta < -1){
        delta_adj = delta + 2
      }else{
        delta_adj <- delta
      }

      if(delta_adj >= 0){
        theta_left = theta1
        theta_right = theta2
      }else if(delta_adj < 0){
        theta_left = theta2
        theta_right = theta1
      }
    }else{
      # subtree only has one child node.
      theta_left <- getNodeAngle.df(df, origin_id, children_ids[1])
      theta_right <- theta_left
    }

  }else{
    # get the real root of df tree to initialise left and right angles.
    tree_root <- getRoot.df(df)
    if( !is.na(tree_root) & is.numeric(tree_root) ){
      theta_left <- getNodeAngle.df(df, origin_id, tree_root)
      theta_right <- theta_left
    }else{
      print('ERROR: no root found!')
      theta_left <- NA
    }

  }

  # no parent angle found.
  if (is.na(theta_left) ){
    return(0)
  }


  # create vector with named columns
  # left-hand and right-hand angles between origin node and the extremities of the tree nodes.
  arc <- c('left' = theta_left, 'right' = theta_right)

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
    # Skip if parent_id is a tip or parent and child node are the same.
    if(origin_id == parent_id | isTip.df(df, parent_id) ){
      next
    }

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

  }

  # Now update tip labels of rotated tree.
  # angle is in range [0, 360]
  for(node in nodes){
    # Update label angle of tipnode if not root node.
    if( isTip.df(df, node) ){
      # get parent
      parent_id <- getParent.df(df, node)
      # if 'node' is not root, then update label angle.
      if( parent_id != 0 ){
        theta_parent_child <- getNodeAngle.df(df, parent_id, node)
        if(!is.na(theta_parent_child)){
          # Update tip label angle, that is parallel to edge.
          #df[node, 'angle'] <- -90 - 180 * theta_parent_child * sign(theta_parent_child - 1)
          if(theta_parent_child > 0 ){
            df[node, 'angle'] <- 180 * theta_parent_child
          }else if(theta_parent_child < 0 ){
            df[node, 'angle'] <- 180 * ( theta_parent_child + 2 )
          }

        }
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
  if( (origin_node_id != node_id) & any(origin_node_id %in% df$node) & any(node_id %in% df$node) ){
    delta_x <- df[node_id, 'x'] - df[origin_node_id, 'x']
    delta_y <- df[node_id, 'y'] - df[origin_node_id, 'y']
    angle <- atan2(delta_y, delta_x) / pi
    return( angle )
  }else{
    return(NA)
  }
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

## Get the distances from the node to all other nodes in data.frame (including itself if in df)
getNodeEuclDistances <- function(df, node){
  # https://stackoverflow.com/questions/24746892/how-to-calculate-euclidian-distance-between-two-points-defined-by-matrix-contain#24747155
  dist <- NULL
  for(i in 1:nrow(df)) dist[i] <- euc.dist(df[df$node==node, c('x', 'y')], df[i, c('x', 'y')])
  return(dist)
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
##' @title getSubtree.df
##' @param df tree data.frame
##' @param node id of starting node.
##' @return list of all child node id's from starting node.
getSubtree.df <- function(df, node){
  subtree <- c(node)
  subtree <- subtree[subtree != 0]
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
##' @title getSubtreeUnrooted
##' @param tree ape phylo tree object
##' @param node is the tree node id from which the subtrees are derived.
##' @return named list of subtrees with the root id of subtree and list of node id's making up subtree.
getSubtreeUnrooted <- function(tree, node){
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
##' @title getSubtreeUnrooted
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
    subtree <- getSubtree.df(df, child)
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
  # Check if root was found.
  if(length(root) == 0){
    # Alternatively, root can self reference, eg node = 10, parent = 10
    root <- unlist(apply(df, 1, function(x){ if(x['node'] == x['parent']){ x['node'] } }))
  }
  return(root)
}








isTip <- function(tr, node) {
  children_ids <- getChild(tr, node)
  #length(children_ids) == 0 ## getChild returns 0 if nothing found.
  if( length(children_ids) == 0 | any(children_ids == 0) ){
    return(TRUE)
  }
  return(FALSE)

}

isTip.df <- function(df, node) {
  # df may not have the isTip structure.
  # return(df[node, 'isTip'])
  # Tip has no children.
  children_ids <- getChild.df(df, node)
  if( length(children_ids) == 0 | any(children_ids == 0) ){
    return(TRUE)
  }
  return(FALSE)
}



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










