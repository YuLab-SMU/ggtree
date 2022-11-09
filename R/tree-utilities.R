

##' @importFrom ape reorder.phylo
layout.unrooted <- function(model, branch.length="branch.length", layout.method="equal_angle", MAX_COUNT=5, ...) {

    df <- switch(layout.method,
                 equal_angle = layoutEqualAngle(model, branch.length),
                 daylight = layoutDaylight(model, branch.length, MAX_COUNT),
    			 ape = layoutApe(model, branch.length)
                 )

    return(df)
}

set_branch_length_cladogram <- function(tree) {
    phylo <- as.phylo(tree)
    edge <- phylo$edge
    xpos <- getXcoord_no_length(phylo)
    phylo$edge.length <- xpos[edge[,2]] - xpos[edge[,1]]

    if (is(tree, "phylo")) {
        return(phylo)
    } else if (is(tree, "treedata")) {
        tree@phylo <- phylo
        return(tree)
    }
    message("unknown tree object, fail to set branch length for cladogram...")
    return(tree)
}

##' 'Equal-angle layout algorithm for unrooted trees'
##'
##' @references
##' "Inferring Phylogenies" by Joseph Felsenstein.
##'
##' @title layoutEqualAngle
##' @param model tree object, e.g. phylo or treedata
##' @param branch.length set to 'none' for edge length of 1. Otherwise the phylogenetic tree edge length is used.
##' @return tree as data.frame with equal angle layout.
layoutEqualAngle <- function(model, branch.length = "branch.length"){
	tree <- as.phylo(model)

  if (! is.null(tree$edge.length)) {
      if (anyNA(tree$edge.length)) {
          cli_alert_warning(c("{.code edge.length} contains NA values...",
                          "## setting {.code edge.length} of the tree to NULL ",
                          "automatically when plotting the tree..."), wrap = TRUE)
          tree$edge.length <- NULL
      }
  }

  if (is.null(tree$edge.length) || branch.length == "none") {
      tree <- set_branch_length_cladogram(tree)
  }
  N <- treeio::Nnode2(tree)
  brlen <- numeric(N)
  brlen[tree$edge[,2]] <- tree$edge.length

  root <- tidytree::rootnode(tree)
  ## Convert Phylo tree to data.frame.
  ## df <- as.data.frame.phylo_(tree)
  df <- as_tibble(model) %>%
      mutate(isTip = ! .data$node %in% .data$parent)

    ## NOTE: Angles (start, end, angle) are in half-rotation units (radians/pi or degrees/180)

    ## create and assign NA to the following fields.
    df$x <- 0
    df$y <- 0
    df$start <- 0 # Start angle of segment of subtree.
    df$end   <- 0 # End angle of segment of subtree
    df$angle <- 0 # Orthogonal angle to beta for tip labels.
    ## Initialize root node position and angles.
    df[root, "x"] <- 0
    df[root, "y"] <- 0
    df[root, "start"] <- 0 # 0-degrees
    df[root, "end"]   <- 2 # 360-degrees
    df[root, "angle"] <- 0 # Angle label.

    df$branch.length <- brlen[df$node] # for cladogram


    ## Get number of tips for each node in tree.
  ## nb.sp <- sapply(1:N, function(i) length(get.offspring.tip(tree, i)))
  ## self_include = TRUE to return itself if the input node is a tip
  nb.sp <- vapply(1:N, function(i) length(offspring(tree, i,  type="tips", self_include = TRUE)), numeric(1))
    ## Get list of node id's.
    nodes <- getNodes_by_postorder(tree)

    for(curNode in nodes) {
        ## Get number of tips for current node.
        curNtip <- nb.sp[curNode]
        ## Get array of child node indexes of current node.
        ## children <- getChild(tree, curNode)
        children <- treeio::child(tree, curNode)

        ## Get "start" and "end" angles of a segment for current node in the data.frame.
        start <- df[curNode, "start"]
        end <- df[curNode, "end"]
        cur_x = df[curNode, "x"]
        cur_y = df[curNode, "y"]

        total_angle = end - start

        for (child in children) {
            ## Get the number of tips for child node.
            ntip.child <- nb.sp[child]

            ## Calculated in half radians.
            ## alpha: angle of segment for i-th child with ntips_ij tips.
            ## alpha = (left_angle - right_angle) * (ntips_ij)/(ntips_current)
            ## alpha <- (end - start) * ntip.child / curNtip
            alpha <- total_angle * ntip.child / curNtip
            ## beta = angle of line from parent node to i-th child.
            beta <- start + alpha / 2

            length.child <- df[child, "branch.length"]

            ## update geometry of data.frame.
            ## Calculate (x,y) position of the i-th child node from current node.
            df[child, "x"] <- cur_x + cospi(beta) * length.child
            df[child, "y"] <- cur_y + sinpi(beta) * length.child
            ## Calculate orthogonal angle to beta for tip label.
            df[child, "angle"] <- -90 - 180 * beta * sign(beta - 1)
            ## Update the start and end angles of the childs segment.
            df[child, "start"] <- start
            df[child, "end"] <- start + alpha
            start <- start + alpha
        }
    }
  tree_df <- as_tibble(df)
  class(tree_df) <- c("tbl_tree", class(tree_df))
  return(tree_df)
}

##' Equal daylight layout method for unrooted trees.
##'
##' #' @title
##' @param model tree object, e.g. phylo or treedata
##' @param branch.length set to 'none' for edge length of 1. Otherwise the phylogenetic tree edge length is used.
##' @param MAX_COUNT the maximum number of iterations to run (default 5)
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
layoutDaylight <- function(model, branch.length, MAX_COUNT=5 ){
	tree <- as.phylo(model)

    ## How to set optimal
    MINIMUM_AVERAGE_ANGLE_CHANGE <- 0.05


    ## Initialize tree.
    tree_df <- layoutEqualAngle(model, branch.length)

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

    ave_change <- 1.0
    for (i in seq_len(MAX_COUNT)) {
        ## Reset max_change after iterating over tree.
        total_max <- 0.0
        for(currentNode_id in nodes){
            result <- applyLayoutDaylight(tree_df, currentNode_id)
            tree_df <- result$tree
            total_max <- total_max + result$max_change
        }
        # Calculate the running average of angle changes.
        ave_change <- total_max / length(nodes)
        message('Average angle change [',i,'] ', ave_change)
        if (ave_change <= MINIMUM_AVERAGE_ANGLE_CHANGE) break
    }

  tree_df <- as_tibble(tree_df)
  class(tree_df) <- c("tbl_tree", class(tree_df))
  return(tree_df)
}

##' Apply the daylight alorithm to adjust the spacing between the subtrees and tips of the
##' specified node.
##'
##' @title applyLayoutDaylight
##' @param df tree data.frame
##' @param node_id is id of the node from which daylight is measured to the other subtrees.
##' @return list with tree data.frame with updated layout using daylight algorithm and max_change angle.
##' @importFrom rlang .data
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
  # Get lists of node ids for each subtree, including  rest of unrooted tree.
  subtrees <- getSubtreeUnrooted.df(df, node_id)

  # Return tree if only 2 or less subtrees to adjust.
  if(length(subtrees) <= 2){
    return( list(tree = df, max_change = 0.0) )
  }

  # Find start and end angles for each subtree.
  #   subtrees = get subtrees of node
  #   for i-th subtree in subtrees {
  angle_list = purrr::map_dfr(subtrees, ~{
    getTreeArcAngles(df, node_id, .x) %>% dplyr::bind_rows()
  }) %>% dplyr::transmute(
    left = .data$left,
    beta = .data$left - .data$right,
    beta = ifelse(.data$beta < 0, .data$beta + 2, .data$beta),
    subtree_id = seq_len(nrow(.))
  ) %>% dplyr::arrange(.data$left)
  #   sort angle_list by 'left angle' column in ascending order.
  #   D = 360 - sum( angle_list['beta'] ) # total day
  #   d = D / |subtrees| # equal daylight angle.
  total_daylight <- 2 - sum(angle_list[['beta']])
  d <- total_daylight / length(subtrees)

  # Initialise new left-angle as first subtree left-angle.
  new_left_angle <- angle_list$left[1]

  # Adjust angles of subtrees and tips connected to current node.
  # for n-th row in angle_list{
  # Skip the first subtree as it is not adjusted.
  max_change <- 0.0
  for (i in 2:nrow(angle_list) ) {
    # Calculate angle to rotate subtree/leaf to create correct daylight angle.
    new_left_angle <- new_left_angle + d + angle_list$beta[i]
    # Calculate the difference between the old and new left angles.
    adjust_angle <- new_left_angle - angle_list$left[i]

    max_change <- max(max_change, abs(adjust_angle))
    #cat('Adjust angle:', abs(adjust_angle), ' Max change:', max_change ,'\n')

    # rotate subtree[index] wrt current node
    subtree_id <- angle_list$subtree_id[i]
    subtree_nodes <- subtrees[[subtree_id]]$subtree
    # update tree_df for all subtrees with rotated points.
    df <- rotateTreePoints.df(df, node_id, subtree_nodes, adjust_angle)
  }

  return( list(tree = df, max_change = max_change) )

}


##' Find the right (clockwise rotation, angle from +ve x-axis to furthest subtree nodes) and
##' left (anti-clockwise angle from +ve x-axis to subtree) Returning arc angle in `[0, 2]` (0 to 360) domain.
##'
##' @title getTreeArcAngles
##' @param df tree data.frame
##' @param origin_id node id from which to calculate left and right hand angles of subtree.
##' @param subtree named list of root id of subtree (node) and list of node ids for given subtree (subtree).
##' @return named list with right and left angles in range `[0,2]` i.e 1 = 180 degrees, 1.5 = 270 degrees.
getTreeArcAngles <- function(df, origin_id, subtree) {
    df_x = df$x
    df_y = df$y
    x_origin = df_x[origin_id]
    y_origin = df_y[origin_id]
    ## Initialise variables
    theta_child <- 0.0
    subtree_root_id <- subtree$node
    subtree_node_ids <- subtree$subtree
    ## Initialise angle from origin node to parent node.
    ## If subtree_root_id is child of origin_id
    ## if (subtree_root_id %in% getChild.df(df, origin_id)) {
    if (subtree_root_id %in% tidytree:::child.tbl_tree(df, origin_id)$node) {
        ## get angle from original node to parent of subtree.
        theta_left <- getNodeAngle.vector(x_origin, y_origin, df_x[subtree_root_id], df_y[subtree_root_id])
        theta_right <- theta_left
    } else if( subtree_root_id == origin_id ){
        ## Special case.
        ## get angle from parent of subtree to children
        ## children_ids <- getChild.df(df, subtree_root_id)
        children_ids <- tidytree:::child.tbl_tree(df, subtree_root_id)$node
        if(length(children_ids) == 2){
            ## get angles from parent to it's two children.
            theta1 <- getNodeAngle.vector(x_origin, y_origin, df_x[children_ids[1]], df_y[children_ids[1]])
            theta2 <- getNodeAngle.vector(x_origin, y_origin, df_x[children_ids[2]], df_y[children_ids[2]])
            delta <- theta1 - theta2
            ## correct delta for points crossing 180/-180 quadrant.
            if(delta > 1){
                delta_adj = delta - 2
            } else if(delta < -1){
                delta_adj = delta + 2
            } else{
                delta_adj <- delta
            }
            if(delta_adj >= 0){
                theta_left = theta1
                theta_right = theta2
            } else if(delta_adj < 0){
                theta_left = theta2
                theta_right = theta1
            }
        }else{
            ## subtree only has one child node.
            theta_left <- getNodeAngle.vector(x_origin, y_origin, df_x[children_ids[1]], df_y[children_ids[1]])
            theta_right <- theta_left
        }
    } else {
        ## get the real root of df tree to initialise left and right angles.
        tree_root <- getRoot.df(df)
        if( !is.na(tree_root) & is.numeric(tree_root) ){
            theta_left <- getNodeAngle.vector(x_origin, y_origin, df_x[tree_root], df_y[tree_root])
            theta_right <- theta_left
        } else{
      print('ERROR: no root found!')
      theta_left <- NA
    }
  }
  # no parent angle found.
  # Subtree has to have 1 or more nodes to compare.
  if (is.na(theta_left) || (length(subtree_node_ids) == 0)){
      return(c('left' = 0, 'right' = 0))
  }
  # create vector with named columns
  # left-hand and right-hand angles between origin node and the extremities of the tree nodes.
    arc <- c('left' = theta_left, 'right' = theta_right)

  # Calculate the angle from the origin node to each child node.
  # Moving from parent to children in depth-first traversal.
  # Skip if parent_id is a tip or parent and child node are the same.
  subtree_node_ids = subtree_node_ids[subtree_node_ids %in% df$parent]
  subtree_node_ids = subtree_node_ids[subtree_node_ids != origin_id]
  for(parent_id in subtree_node_ids){
    # Get angle from origin node to parent node.
    theta_parent <- getNodeAngle.vector(x_origin, y_origin, df_x[parent_id], df_y[parent_id])
      ## children_ids <- getChild.df(df, parent_id)
      children_ids <- tidytree:::child.tbl_tree(df, parent_id)$node
    # Skip if child is parent node of subtree.
    children_ids = children_ids[children_ids != origin_id]
    for(child_id in children_ids){
      theta_child <- getNodeAngle.vector(x_origin, y_origin, df_x[child_id], df_y[child_id])
      # Skip if child node is already inside arc.
      # if left < right angle (arc crosses 180/-180 quadrant) and child node is not inside arc of tree.
      # OR if left > right angle (arc crosses 0/360 quadrant) and child node is inside gap
      if ((arc['left'] < arc['right'] & !(theta_child > arc['left'] & theta_child < arc['right'])) |
          (arc['left'] > arc['right'] &  (theta_child < arc['left'] & theta_child > arc['right'])) ){
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
  arc
}

##' Rotate the points in a tree data.frame around a pivot node by the angle specified.
##'
##' @title rotateTreePoints.data.frame
##' @rdname rotateTreePoints
##' @param df tree data.frame
##' @param pivot_node is the id of the pivot node.
##' @param nodes list of node numbers that are to be rotated by angle around the pivot_node
##' @param angle in range `[0,2]`, ie degrees/180, radians/pi
##' @return updated tree data.frame with points rotated by angle
rotateTreePoints.df <- function(df, pivot_node, nodes, angle){
  # Rotate nodes around pivot_node.
  # x' = cos(angle)*delta_x - sin(angle)*delta_y + delta_x
  # y' = sin(angle)*delta_x + cos(angle)*delta_y + delta_y
  cospitheta <- cospi(angle)
  sinpitheta <- sinpi(angle)
  pivot_x = df$x[pivot_node]
  pivot_y = df$y[pivot_node]
  delta_x = df$x - pivot_x
  delta_y = df$y - pivot_y
  df = mutate(df,
    x = ifelse(.data$node %in% nodes, cospitheta * delta_x - sinpitheta * delta_y + pivot_x, .data$x),
    y = ifelse(.data$node %in% nodes, sinpitheta * delta_x + cospitheta * delta_y + pivot_y, .data$y)
  )
  x_parent = df$x[df$parent]
  y_parent = df$y[df$parent]
  # Now update tip labels of rotated tree.
  # angle is in range [0, 360]
  # Update label angle of tipnode if not root node.
  nodes = nodes[! nodes %in% df$parent]
  df %>% mutate(
    angle = ifelse(.data$node %in% nodes,
       getNodeAngle.vector(x_parent, y_parent, .data$x, .data$y) %>%
         {180 * ifelse(. < 0, 2 + ., .)},
       .data$angle)
  )
}

##' Get the angle between the two nodes specified.
##'
##' @title getNodeAngle.df
##' @param df tree data.frame
##' @param origin_node_id origin node id number
##' @param node_id end node id number
##' @return angle in range `[-1, 1]`, i.e. degrees/180, radians/pi
getNodeAngle.df <- function(df, origin_node_id, node_id){
  if (origin_node_id != node_id) {
    df_x = df$x
    df_y = df$y
    atan2(df_y[node_id] - df_y[origin_node_id], df_x[node_id] - df_x[origin_node_id]) / pi
  }else{
    NA
  }
}

getNodeAngle.vector <- function(x_origin, y_origin, x, y) {
  atan2(y - y_origin, x - x_origin) / pi
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

  ## subtree <- c(node)
  ## i <- 1
  ## while( i <= length(subtree)){
  ##   subtree <- c(subtree, treeio::child(tree, subtree[i]))
  ##   # remove any '0' root nodes
  ##   subtree <- subtree[subtree != 0]
  ##   i <- i + 1
  ## }
    ## return(subtree)
    tidytree::offspring(tree, node, self_include = TRUE)
}

##' Get all children of node from df tree using breath-first.
##'
##' @title getSubtree.df
##' @param df tree data.frame
##' @param node id of starting node.
##' @return list of all child node id's from starting node.
getSubtree.df <- function(df, node){
  ## subtree <- node[node != 0]
  ## i <- 1
  ## while( i <= length(subtree)){
  ##     ## subtree <- c(subtree, getChild.df(df, subtree[i]))
  ##     subtree <- c(subtree, tidytree::child(df, subtree[i])$node)
  ##   i <- i + 1
  ## }
    ## subtree
    #tidytree:::offspring.tbl_tree(df, node, self_include = TRUE)$node
    offspring.tbl_tree(df, node, self_include = TRUE)$node
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
  if( treeio::isTip(tree, node) ){
    # return NA
    return(NA)
  }

  subtrees <- list()

  # get subtree for each child node.
    ## children_ids <- getChild(tree, node)
    children_ids <- treeio::child(tree, node)

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
  parent_id <- parent(tree, node)
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
##' @importFrom tidytree parent
##' @return named list of subtrees with the root id of subtree and list of node id's making up subtree.
getSubtreeUnrooted.df <- function(df, node){
  # get subtree for each child node.
                                        # children_ids <- getChild.df(df, node)
    children_ids <- child.tbl_tree(df, node)$node
  if (length(children_ids) == 0L) return(NULL)
  # if node leaf, return nothing.

  subtrees = tibble::tibble(
    node = children_ids,
    subtree = purrr::map(.data$node, ~getSubtree.df(df, .x))
  )
  remaining_nodes = setdiff(df$node, purrr::flatten_int(subtrees$subtree))

  # The remaining nodes that are not found in the child subtrees are the remaining subtree nodes.
  # ie, parent node and all other nodes. We don't care how they are connected, just their id.
  parent_id <- parent.tbl_tree(df, node)$node
  # If node is not root.
  if ((length(parent_id) > 0) & (length(remaining_nodes) > 0)) {
    subtrees = tibble::add_row(subtrees, node = parent_id, subtree = list(remaining_nodes))
  }
  purrr::transpose(subtrees)
}


getRoot.df <- function(df, node){

  root <- which(is.na(df$parent))
  # Check if root was found.
  if(length(root) == 0){
      ## Alternatively, root can self reference, eg node = 10, parent = 10
      root <- df$node[df$parent == df$node]
      ## root <- unlist(apply(df, 1, function(x){ if(x['node'] == x['parent']){ x['node'] } }))
  }
  return(root)
}

##' Get the nodes of tree from root in breadth-first order.
##'
##' @title getNodesBreadthFirst.df
##' @param df tree data.frame
##' @return list of node id's in breadth-first order.
getNodesBreadthFirst.df <- function(df){

  root <- getRoot.df(df)
  if(treeio::isTip(df, root)){
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
    if(treeio::isTip(df, parent)){
      next
    }

    # get children of current parent.
    children <- tidytree::child(df,parent)$node

    # add children to result
    res <- c(res, children)

  }

  return(res)

}


isRoot <- function(tr, node) {
    getRoot(tr) == node
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
##' @importFrom tidytree ancestor
##' @export
##' @author Guangchuang Yu
get.path <- function(phylo, from, to) {
    anc_from <- ancestor(phylo, from)
    anc_from <- c(from, anc_from)
    anc_to <- ancestor(phylo, to)
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

##' @importFrom ape reorder.phylo
getNodes_by_postorder <- function(tree) {
  tree <- reorder.phylo(tree, "postorder")
    unique(rev(as.vector(t(tree$edge[,c(2,1)]))))
}

getXcoord2 <- function(x, root, parent, child, len, start=0, rev=FALSE) {
    x[root] <- start
    x[-root] <- NA  ## only root is set to start, by default 0

    currentNode <- root
    direction <- 1
    if (rev == TRUE) {
        direction <- -1
    }

    ignore_negative_edge <- getOption("ignore.negative.edge", default=FALSE)

    if (any(len < 0) && !ignore_negative_edge) {
        cli_alert_warning(c("The tree contained negative ", ifelse(sum(len < 0)>1, "edge lengths", "edge length"), 
                        ". If you want to ignore the ", ifelse(sum(len<0) > 1, "edges", "edge"), ", you can 
                        set {.code options(ignore.negative.edge=TRUE)}, then re-run ggtree."
                     ), wrap = TRUE)
    }
    while(anyNA(x)) {
        idx <- which(parent %in% currentNode)
        newNode <- child[idx]
        if (ignore_negative_edge){
            x[newNode] <- x[parent[idx]]+len[idx] * direction * sign(len[idx])
        } else {
            x[newNode] <- x[parent[idx]]+len[idx] * direction
        }
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




## scale the branch (the line plotted) to the actual value of edge length
## but it seems not the good idea as if we want to add x-axis (e.g. time-scaled tree)
## then the x-value is not corresponding to edge length as in rectangular layout
## getXYcoord_slanted <- function(tr) {
##     edge <- tr$edge
##     parent <- edge[,1]
##     child <- edge[,2]
##     root <- getRoot(tr)

##     N <- getNodeNum(tr)
##     len <- tr$edge.length
##     y <- getYcoord(tr, step=min(len)/2)
##     len <- sqrt(len^2 - (y[parent]-y[child])^2)
##     x <- numeric(N)
##     x <- getXcoord2(x, root, parent, child, len)
##     res <- data.frame(x=x, y=y)
##     return(res)
## }



## @importFrom magrittr %>%
##' @importFrom magrittr equals
getYcoord <- function(tr, step=1, tip.order = NULL) {
    Ntip <- length(tr[["tip.label"]])
    N <- getNodeNum(tr)

    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]

    cl <- split(child, parent)
    child_list <- list()
    child_list[as.numeric(names(cl))] <- cl

    y <- numeric(N)
    if (is.null(tip.order)) {
        tip.idx <- child[child <= Ntip]
        y[tip.idx] <- 1:Ntip * step
    } else {
        tip.idx <- 1:Ntip
        y[tip.idx] <- match(tr$tip.label, tip.order) * step
    }
    y[-tip.idx] <- NA


    pvec <- edge2vec(tr)

    currentNode <- 1:Ntip
    while(anyNA(y)) {
        ## pNode <- unique(parent[child %in% currentNode])
        pNode <- unique(pvec[currentNode])

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
        sib <- tidytree::sibling(tr, ordered_tip[ii])
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


    long_branch <- ancestor(tr, ordered_tip[1]) %>% rev
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
    yy <- df[[variable]]
    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
        tree <- get.tree(tr)
        nodes <- getNodes_by_postorder(tree)
        for (curNode in nodes) {
            children <- treeio::child(tree, curNode)
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
    yy <- df[[variable]]
    if (!is.null(yscale_mapping)) {
        yy <- yscale_mapping[yy]
    }

    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
        tree <- get.tree(tr)
        nodes <- rev(getNodes_by_postorder(tree))
        for (curNode in nodes) {
            parent <- parent(tree, curNode)
            if (parent == 0) { ## already reach root
                next
            }
            idx <- which(is.na(yy[parent]))
            if (length(idx) > 0) {
                child <- treeio::child(tree, parent)
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
        yy <- df[[yscale]]
        ii <- which(is.na(yy))
        if (length(ii)) {
            ## df[ii, yscale] <- df[ii, "node"]
            df[[yscale]][ii] <- as.character(df[['node']][ii])
        }
    }

    ## assign to parent status is more prefer...
    df <- .assign_parent_status(tr, df, yscale)
    df <- .assign_child_status(tr, df, yscale, yscale_mapping)

    y <- df[[yscale]]

    if (anyNA(y)) {
        warning("NA found in y scale mapping, all were setting to 0")
        y[is.na(y)] <- 0
    }
    return(y)
}


add_angle_slanted <- function(res) {
    x <- res[["x"]]
    y <- res[["y"]]
    dy <- (y - y[match(res$parent, res$node)]) / diff(range(y))
    dx <- (x - x[match(res$parent, res$node)]) / diff(range(x))
    theta <- atan(dy/dx)
    theta[is.na(theta)] <- 0 ## root node
    res$angle <- theta/pi * 180

    branch.y <- (y[match(res$parent, res$node)] + y)/2
    idx <- is.na(branch.y)
    branch.y[idx] <- y[idx]
    res[, "branch.y"] <- branch.y
    return(res)
}


calculate_branch_mid <- function(res, layout) {
    if (layout %in% c("equal_angle", "daylight", "ape")){
        res$branch.y <- with(res, (y[match(parent, node)] + y)/2)
        res$branch.y[is.na(res$branch.y)] <- 0
    }
    res$branch <- with(res, (x[match(parent, node)] + x)/2)
    if (!is.null(res[['branch.length']])) {
        res$branch.length[is.na(res$branch.length)] <- 0
    }
    res$branch[is.na(res$branch)] <- 0
    if (layout %in% c("equal_angle", "daylight", "ape")){
        res$branch.x <- res$branch
    }
    return(res)
}


re_assign_ycoord_df <- function(df, currentNode) {
    while(anyNA(df$y)) {
        pNode <- with(df, parent[match(currentNode, node)]) %>% unique
        idx <- sapply(pNode, function(i) with(df, all(node[parent == i & parent != node] %in% currentNode)))
        newNode <- pNode[idx]
        ## newNode <- newNode[is.na(df[match(newNode, df$node), "y"])]
        if (length(newNode) == 0)
            break
        df[match(newNode, df$node), "y"] <- sapply(newNode, function(i) {
            with(df, mean(y[parent == i], na.rm = TRUE))
        })
        traced_node <- as.vector(sapply(newNode, function(i) with(df, node[parent == i])))
        currentNode <- c(currentNode[! currentNode %in% traced_node], newNode)
    }
    return(df)
}


layoutApe <- function(model, branch.length="branch.length") {
	tree <- as.phylo(model) %>% stats::reorder("postorder")

	if (! is.null(tree$edge.length)) {
		if (anyNA(tree$edge.length)) {
			warning("'edge.length' contains NA values...\n## setting 'edge.length' to NULL automatically when plotting the tree...")
			tree$edge.length <- NULL
		}
	}

	if (is.null(tree$edge.length) || branch.length == "none") {
		tree <- set_branch_length_cladogram(tree)
	}

	edge <- tree$edge
	edge.length <- tree$edge.length
	nb.sp <- ape::node.depth(tree)

	df <- as_tibble(model) %>%
		mutate(isTip = ! .data$node %in% .data$parent)
	#df$branch.length <- edge.length[df$node] # for cladogram

	# unrooted layout from cran/ape
	M <- ape::unrooted.xy(Ntip(tree),
						  Nnode(tree),
						  tree$edge,
						  tree$edge.length,
						  nb.sp,
						  0)$M
	xx <- M[, 1]
	yy <- M[, 2]

	M <- tibble::tibble(
		node = 1:(Ntip(tree) + Nnode(tree)),
		x = xx - min(xx),
		y = yy - min(yy)
	)

	tree_df <- dplyr::full_join(df, M, by = "node") %>%
		as_tibble()
	class(tree_df) <- c("tbl_tree", class(tree_df))
	tree_df
}

.nodeId <- function (tree, type = "all"){
    type <- match.arg(type, c("all", "tips", "internal"))
    if (inherits(tree, "treedata")) {
        tree <- tree@phylo
    }
    nodes <- unique(as.vector(tree$edge))
    if (type == "all") {
        return(nodes)
    }
    edge <- tree$edge
    tips <- edge[!edge[, 2] %in% edge[, 1], 2]
    if (type == "tips"){
        return(tips)
    }
    else if (type == "internal") {
        return(setdiff(nodes, tips))
    }
}

.convert_tips2ancestors_sbp <- function (tree, include.root = FALSE, type = "all", include.self = TRUE){
    all.nodes <- .nodeId(tree)
    if (!include.root) {
        all.nodes <- setdiff(all.nodes, treeio::rootnode(tree))
    }
    tip.nodes <- .nodeId(tree, type = "tips")
    sbp <- lapply(tip.nodes, 
                       .internal_ancestor, 
                       .data = tree, 
                       all.nodes = all.nodes,
                       type = type, 
                       include.self = include.self
                ) %>%
           stats::setNames(tip.nodes) %>% 
           do.call(rbind, .) 
    colnames(sbp) <- all.nodes
    return(sbp)
}

.internal_ancestor <- function(.data, .node, all.nodes, type = 'all', include.self=TRUE){
    .internal_anc <- switch(type, all = treeio::ancestor, parent = treeio::parent)
    x <- .internal_anc(.data=.data, .node=.node)
    if (include.self){
        x <- c(x, .node)
    }
    x <- all.nodes %in% x
    return (x)
}


getXcoord_no_length_slanted <- function(x){
    x <- -colSums(x)
    x <- unname(x[order(as.numeric(names(x)))])
    x <- x + max(abs(x))
    return(x)
}

getYcoord_no_length_slanted <- function(y){
    y <- seq_len(nrow(y)) * y
    y[y==0] <- NA
    y <- colMeans(y, na.rm = TRUE)
    y <- unname(y[order(as.numeric(names(y)))])
    return(y)
}


edge2vec <- function(tr) {
  parent <- tr$edge[,1]
  child <- tr$edge[,2]
  
  ## use lookup table
  pvec <- integer(max(tr$edge))
  pvec[child] <- parent
  return(pvec)
}



as.phylo.hclust2 <- function(x, hang=0.1, ...) {
  h <- x
  tr <- ape::as.phylo(x)
  ev <- edge2vec(tr)

  #extract_inode_hclust_item <- function(h, i, ev) {
  #  j <- h$merge[i,]
  #  if (any(j < 0)) {
  #    j2 <- j[j < 0][1]
  #    res <- ev[abs(j2)]
  #  } else {
  #    res <- ev[extract_inode_hclust_item(h, j, ev)]
  #  }
  #  return(res)
  #}

  #nodes <- vapply(seq_along(h$height), function(i) {
  #  extract_inode_hclust_item(h, i, ev)
  #}, numeric(1))
  
  nodes <- integer(length(h$height))
  for (i in seq_along(nodes)) {
    j <- h$merge[i,]
    if (any(j < 0)) {
      j2 <- j[j < 0][1]
      nodes[i] <- ev[abs(j2)]
    } else {
      nodes[i] <- ev[nodes[j[1]]]
    }
  }

  #len <- numeric(max(tr$edge))
  #len[nodes] <- h$height
  #pn <- ev[nodes]
  #pn[pn == 0] <- treeio::rootnode(tr)
  #len[nodes] <- len[pn] - len[nodes]
  #len[1:Ntip(tr)] <- hang #max(h$height)/10

  #tr$edge.length <- len[tr$edge[,2]]

  tip2parent <- tr$edge[match(seq_len(Ntip(tr)), tr$edge[,2]), 1]
  if (hang > 0){
    tip.edge.len <- hang * max(h$height) - h$height[match(tip2parent, nodes)]
    attr(tr,'tip.edge.len') <- tip.edge.len
  }
  tr$edge.length <- tr$edge.length * 2
  return(tr)
}


