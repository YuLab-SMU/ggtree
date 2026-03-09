

##' @importFrom ape reorder.phylo
layout.unrooted <- function(model, branch.length="branch.length", layout.method="equal_angle", MAX_COUNT=5, ...) {

    df <- switch(layout.method,
                 equal_angle = layoutEqualAngle(model, branch.length),
                 daylight = layoutDaylight(model, branch.length, MAX_COUNT),
                 tree_and_leaf = layoutTreeAndLeaf(model, branch.length, MAX_COUNT = MAX_COUNT, ...),
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
    cache <- .daylightBuildCache(tree_df)
    nodes <- cache$internal_nodes

    if (length(nodes) == 0L) {
        tree_df <- as_tibble(tree_df)
        class(tree_df) <- c("tbl_tree", class(tree_df))
        return(tree_df)
    }

    ave_change <- 1.0
    for (i in seq_len(MAX_COUNT)) {
        total_max <- 0.0
        for (currentNode_id in nodes) {
            result <- applyLayoutDaylight(tree_df, currentNode_id, cache = cache)
            tree_df <- result$tree
            total_max <- total_max + result$max_change
        }
        ave_change <- total_max / length(nodes)
        message('Average angle change [', i, '] ', ave_change)
        if (ave_change <= MINIMUM_AVERAGE_ANGLE_CHANGE) break
    }

    tree_df <- as_tibble(tree_df)
    class(tree_df) <- c("tbl_tree", class(tree_df))
    return(tree_df)
}

##' TreeAndLeaf-inspired unrooted layout.
##'
##' @param model tree object, e.g. phylo or treedata
##' @param branch.length set to 'none' for edge length of 1. Otherwise the phylogenetic tree edge length is used.
##' @param MAX_COUNT maximum number of iterations used by the daylight initializer.
##' @param initial_layout initial unrooted layout used before leaf-centric relaxation.
##' @param max_iter maximum number of relaxation iterations.
##' @param leaf_force pairwise repulsion strength among leaves.
##' @param edge_force spring strength used to preserve the tree skeleton.
##' @param anchor_force anchor strength pulling tips toward the initial layout.
##' @param internal_anchor anchor strength pulling internal nodes toward the initial layout.
##' @param outward_force outward radial bias applied to leaves.
##' @param step initial step size.
##' @param cooling multiplicative decay applied to step size per iteration.
##' @param tol convergence tolerance on maximum displacement.
##' @return tree as data.frame with a TreeAndLeaf-inspired layout.
layoutTreeAndLeaf <- function(model,
                              branch.length,
                              MAX_COUNT = 5,
                              initial_layout = "daylight",
                              max_iter = 200L,
                              leaf_force = 0.08,
                              edge_force = 0.25,
                              anchor_force = 0.02,
                              internal_anchor = 0.08,
                              outward_force = 0.01,
                              step = 0.2,
                              cooling = 0.98,
                              tol = 1e-4,
                              ...) {
    initial_layout <- match.arg(initial_layout, c("daylight", "equal_angle", "ape"))
    max_iter <- as.integer(max_iter)
    if (is.na(max_iter) || max_iter < 1L) {
        stop("`max_iter` must be a positive integer.")
    }

    tree_df <- switch(
        initial_layout,
        daylight = suppressMessages(layoutDaylight(model, branch.length, MAX_COUNT = MAX_COUNT)),
        equal_angle = layoutEqualAngle(model, branch.length),
        ape = layoutApe(model, branch.length)
    )

    cache <- .treeAndLeafBuildCache(tree_df)
    if (length(cache$tip_rows) <= 1L) {
        tree_df <- as_tibble(tree_df)
        class(tree_df) <- c("tbl_tree", class(tree_df))
        return(tree_df)
    }

    positions <- as.matrix(tree_df[, c("x", "y")])
    initial_positions <- positions
    current_step <- step
    n_nodes <- nrow(tree_df)

    for (iter in seq_len(max_iter)) {
        forces <- matrix(0, nrow = n_nodes, ncol = 2)

        tip_rows <- cache$tip_rows
        tip_weights <- 0.5 + cache$depth_norm[tip_rows]
        n_tips <- length(tip_rows)
        for (i in seq_len(n_tips - 1L)) {
            row_i <- tip_rows[i]
            other_rows <- tip_rows[(i + 1L):n_tips]
            delta <- sweep(positions[other_rows, , drop = FALSE], 2, positions[row_i, ], FUN = "-")
            dist2 <- rowSums(delta^2)
            zero_idx <- dist2 < 1e-12
            if (any(zero_idx)) {
                delta[zero_idx, ] <- .treeAndLeafFallbackDirections(
                    cache$node_ids[row_i],
                    cache$node_ids[other_rows[zero_idx]]
                )
                dist2[zero_idx] <- rowSums(delta[zero_idx, , drop = FALSE]^2)
            }
            dist <- sqrt(dist2)
            magnitude <- leaf_force * (tip_weights[i] + tip_weights[(i + 1L):n_tips]) / (dist2 + 1e-6)
            contribution <- delta / dist * magnitude
            forces[row_i, ] <- forces[row_i, ] - colSums(contribution)
            forces[other_rows, ] <- forces[other_rows, ] + contribution
        }

        for (edge_idx in seq_along(cache$edge_child_rows)) {
            parent_row <- cache$edge_parent_rows[edge_idx]
            child_row <- cache$edge_child_rows[edge_idx]
            delta <- positions[child_row, ] - positions[parent_row, ]
            dist2 <- sum(delta^2)
            if (dist2 < 1e-12) {
                delta <- .treeAndLeafFallbackDirections(
                    cache$node_ids[parent_row],
                    cache$node_ids[child_row]
                )[1, ]
                dist2 <- sum(delta^2)
            }
            dist <- sqrt(dist2)
            magnitude <- edge_force * (dist - cache$target_edge_length[edge_idx])
            contribution <- delta / dist * magnitude
            forces[parent_row, ] <- forces[parent_row, ] + contribution
            forces[child_row, ] <- forces[child_row, ] - contribution
        }

        anchor_weights <- rep(anchor_force, n_nodes)
        anchor_weights[cache$internal_rows] <- internal_anchor
        anchor_weights[cache$root_row] <- 0
        forces <- forces + (initial_positions - positions) * anchor_weights

        if (outward_force != 0) {
            outward_scale <- outward_force * (0.5 + cache$depth_norm)
            forces[cache$tip_rows, ] <- forces[cache$tip_rows, , drop = FALSE] +
                cache$outward_dir[cache$tip_rows, , drop = FALSE] * outward_scale[cache$tip_rows]
        }

        displacement <- forces * (current_step * cache$mobility)
        displacement_norm <- sqrt(rowSums(displacement^2))
        limit_idx <- displacement_norm > current_step & displacement_norm > 0
        if (any(limit_idx)) {
            displacement[limit_idx, ] <- displacement[limit_idx, , drop = FALSE] *
                (current_step / displacement_norm[limit_idx])
        }

        positions <- positions + displacement
        positions[cache$root_row, ] <- initial_positions[cache$root_row, ]

        if (max(displacement_norm, na.rm = TRUE) < tol) {
            break
        }
        current_step <- current_step * cooling
    }

    tree_df$x <- positions[, 1]
    tree_df$y <- positions[, 2]
    tree_df <- as_tibble(tree_df)
    class(tree_df) <- c("tbl_tree", class(tree_df))
    tree_df
}

.treeAndLeafBuildCache <- function(df) {
    node_ids <- as.integer(df$node)
    parent_ids <- as.integer(df$parent)
    n_nodes <- nrow(df)
    max_node <- max(node_ids, na.rm = TRUE)
    row_index <- rep.int(NA_integer_, max_node)
    row_index[node_ids] <- seq_len(n_nodes)

    root_row <- which(is.na(parent_ids) | parent_ids == node_ids)[1]
    if (length(root_row) == 0L || is.na(root_row)) {
        stop("Unable to determine the tree root for `tree_and_leaf` layout.")
    }

    parent_rows <- row_index[parent_ids]
    edge_child_rows <- setdiff(seq_len(n_nodes), root_row)
    edge_parent_rows <- parent_rows[edge_child_rows]

    target_edge_length <- sqrt(
        (df$x[edge_child_rows] - df$x[edge_parent_rows])^2 +
        (df$y[edge_child_rows] - df$y[edge_parent_rows])^2
    )
    target_edge_length[target_edge_length < 1e-8] <- 1e-8

    edge_length_by_row <- numeric(n_nodes)
    edge_length_by_row[edge_child_rows] <- target_edge_length
    depth <- rep(NA_real_, n_nodes)
    depth[root_row] <- 0
    unresolved <- setdiff(seq_len(n_nodes), root_row)
    while (length(unresolved) > 0L) {
        progressed <- FALSE
        for (row in unresolved) {
            parent_row <- parent_rows[row]
            if (!is.na(depth[parent_row])) {
                depth[row] <- depth[parent_row] + edge_length_by_row[row]
                progressed <- TRUE
            }
        }
        unresolved <- which(is.na(depth))
        if (!progressed) {
            depth[is.na(depth)] <- 0
            break
        }
    }
    max_depth <- max(depth, na.rm = TRUE)
    depth_norm <- if (max_depth > 0) depth / max_depth else rep(0, n_nodes)

    outward_dir <- cbind(df$x - df$x[root_row], df$y - df$y[root_row])
    dir_norm <- sqrt(rowSums(outward_dir^2))
    fallback_rows <- dir_norm < 1e-12
    if (any(fallback_rows)) {
        fallback <- .treeAndLeafFallbackDirections(node_ids[fallback_rows], node_ids[fallback_rows] + 1L)
        outward_dir[fallback_rows, ] <- fallback
        dir_norm[fallback_rows] <- sqrt(rowSums(fallback^2))
    }
    outward_dir <- outward_dir / dir_norm

    tip_rows <- which(df$isTip)
    internal_rows <- which(!df$isTip)
    mobility <- rep(0.18, n_nodes)
    mobility[tip_rows] <- 0.75 + 0.25 * depth_norm[tip_rows]
    mobility[root_row] <- 0

    list(
        node_ids = node_ids,
        root_row = root_row,
        tip_rows = tip_rows,
        internal_rows = internal_rows,
        edge_parent_rows = edge_parent_rows,
        edge_child_rows = edge_child_rows,
        target_edge_length = target_edge_length,
        depth_norm = depth_norm,
        outward_dir = outward_dir,
        mobility = mobility
    )
}

.treeAndLeafFallbackDirections <- function(node_ids, other_ids) {
    angles <- (((as.numeric(node_ids) + as.numeric(other_ids)) * 0.618033988749895) %% 1) * 2 * pi
    cbind(cos(angles), sin(angles))
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
applyLayoutDaylight <- function(df, node_id, cache = NULL){
  if (is.null(cache)) {
    cache <- .daylightBuildCache(df)
  }

  subtrees <- cache$subtrees[[node_id]]
  if (is.null(subtrees) || length(subtrees) <= 2L) {
    return(list(tree = df, max_change = 0.0))
  }

  n_subtrees <- length(subtrees)
  angle_left <- numeric(n_subtrees)
  angle_beta <- numeric(n_subtrees)

  for (i in seq_len(n_subtrees)) {
    arc <- .daylightGetArcAngles(df, node_id, subtrees[[i]], cache)
    angle_left[i] <- arc[['left']]
    beta <- arc[['left']] - arc[['right']]
    if (beta < 0) {
      beta <- beta + 2
    }
    angle_beta[i] <- beta
  }

  order_idx <- order(angle_left)
  angle_left <- angle_left[order_idx]
  angle_beta <- angle_beta[order_idx]

  total_daylight <- 2 - sum(angle_beta)
  d <- total_daylight / n_subtrees
  new_left_angle <- angle_left[1]

  max_change <- 0.0
  for (i in 2:n_subtrees) {
    new_left_angle <- new_left_angle + d + angle_beta[i]
    adjust_angle <- new_left_angle - angle_left[i]
    max_change <- max(max_change, abs(adjust_angle))
    subtree_nodes <- subtrees[[order_idx[i]]]$subtree
    df <- .daylightRotatePoints(df, node_id, subtree_nodes, adjust_angle, cache)
  }

  list(tree = df, max_change = max_change)
}

.daylightBuildCache <- function(df) {
  node_ids <- as.integer(df$node)
  parent_ids <- as.integer(df$parent)
  max_node <- max(node_ids, na.rm = TRUE)

  row_index <- rep.int(NA_integer_, max_node)
  row_index[node_ids] <- seq_along(node_ids)

  children <- vector('list', max_node)
  has_parent <- !is.na(parent_ids) & parent_ids != node_ids
  if (any(has_parent)) {
    split_children <- split(node_ids[has_parent], parent_ids[has_parent])
    for (i in seq_along(split_children)) {
      parent_id <- as.integer(names(split_children)[i])
      children[[parent_id]] <- unname(as.integer(split_children[[i]]))
    }
  }
  empty_children <- lengths(children) == 0L
  children[empty_children] <- list(integer())

  has_children <- lengths(children) > 0L
  is_tip <- rep.int(FALSE, max_node)
  is_tip[node_ids] <- !node_ids %in% parent_ids[has_parent]

  root_rows <- which(is.na(df$parent) | df$parent == df$node)
  root_node <- if (length(root_rows) > 0L) node_ids[root_rows[1]] else NA_integer_

  bfs_nodes <- integer()
  if (!is.na(root_node)) {
    bfs_nodes <- root_node
    i <- 1L
    while (i <= length(bfs_nodes)) {
      child_ids <- children[[bfs_nodes[i]] ]
      i <- i + 1L
      if (length(child_ids) > 0L) {
        bfs_nodes <- c(bfs_nodes, child_ids)
      }
    }
  }
  internal_nodes <- bfs_nodes[has_children[bfs_nodes]]

  subtrees <- vector('list', max_node)
  for (node_id in internal_nodes) {
    child_ids <- children[[node_id]]
    node_subtrees <- lapply(child_ids, function(child_id) {
      list(node = child_id, subtree = getSubtree.df(df, child_id))
    })

    covered_nodes <- unlist(lapply(node_subtrees, `[[`, 'subtree'), use.names = FALSE)
    remaining_nodes <- setdiff(node_ids, covered_nodes)
    parent_id <- parent_ids[row_index[node_id]]
    if (!is.na(parent_id) && parent_id != node_id && length(remaining_nodes) > 0L) {
      node_subtrees[[length(node_subtrees) + 1L]] <- list(node = parent_id, subtree = remaining_nodes)
    }
    subtrees[[node_id]] <- node_subtrees
  }

  list(
    row_index = row_index,
    children = children,
    has_children = has_children,
    is_tip = is_tip,
    subtrees = subtrees,
    root_node = root_node,
    internal_nodes = internal_nodes
  )
}

.daylightGetArcAngles <- function(df, origin_id, subtree, cache) {
  df_x <- df$x
  df_y <- df$y
  row_index <- cache$row_index
  origin_row <- row_index[origin_id]
  x_origin <- df_x[origin_row]
  y_origin <- df_y[origin_row]
  subtree_root_id <- subtree$node
  subtree_node_ids <- subtree$subtree
  children_ids <- cache$children[[origin_id]]

  theta_left <- NA_real_
  theta_right <- NA_real_

  if (subtree_root_id %in% children_ids) {
    subtree_root_row <- row_index[subtree_root_id]
    theta_left <- getNodeAngle.vector(x_origin, y_origin, df_x[subtree_root_row], df_y[subtree_root_row])
    theta_right <- theta_left
  } else if (subtree_root_id == origin_id) {
    if (length(children_ids) == 0L) {
      return(c(left = 0, right = 0))
    }
    if (length(children_ids) == 2L) {
      child_rows <- row_index[children_ids]
      theta1 <- getNodeAngle.vector(x_origin, y_origin, df_x[child_rows[1]], df_y[child_rows[1]])
      theta2 <- getNodeAngle.vector(x_origin, y_origin, df_x[child_rows[2]], df_y[child_rows[2]])
      delta <- theta1 - theta2
      if (delta > 1) {
        delta_adj <- delta - 2
      } else if (delta < -1) {
        delta_adj <- delta + 2
      } else {
        delta_adj <- delta
      }
      if (delta_adj >= 0) {
        theta_left <- theta1
        theta_right <- theta2
      } else {
        theta_left <- theta2
        theta_right <- theta1
      }
    } else {
      child_row <- row_index[children_ids[1]]
      theta_left <- getNodeAngle.vector(x_origin, y_origin, df_x[child_row], df_y[child_row])
      theta_right <- theta_left
    }
  } else {
    tree_root <- cache$root_node
    if (!is.na(tree_root)) {
      tree_root_row <- row_index[tree_root]
      theta_left <- getNodeAngle.vector(x_origin, y_origin, df_x[tree_root_row], df_y[tree_root_row])
      theta_right <- theta_left
    } else {
      return(c(left = 0, right = 0))
    }
  }

  if (is.na(theta_left) || length(subtree_node_ids) == 0L) {
    return(c(left = 0, right = 0))
  }

  arc <- c(left = theta_left, right = theta_right)
  parent_nodes <- subtree_node_ids[subtree_node_ids != origin_id]
  parent_nodes <- parent_nodes[cache$has_children[parent_nodes]]

  for (parent_id in parent_nodes) {
    parent_row <- row_index[parent_id]
    theta_parent <- getNodeAngle.vector(x_origin, y_origin, df_x[parent_row], df_y[parent_row])
    child_ids <- cache$children[[parent_id]]
    child_ids <- child_ids[child_ids != origin_id]
    if (length(child_ids) == 0L) {
      next
    }

    for (child_id in child_ids) {
      child_row <- row_index[child_id]
      theta_child <- getNodeAngle.vector(x_origin, y_origin, df_x[child_row], df_y[child_row])
      if ((arc['left'] < arc['right'] && !(theta_child > arc['left'] && theta_child < arc['right'])) ||
          (arc['left'] > arc['right'] && (theta_child < arc['left'] && theta_child > arc['right']))) {
        next
      }

      delta <- theta_child - theta_parent
      delta_adj <- delta
      if (delta > 1) {
        delta_adj <- delta - 2
      } else if (delta < -1) {
        delta_adj <- delta + 2
      }

      theta_child_adj <- theta_child
      if (delta_adj > 0) {
        if (abs(delta) > 1) {
          if (arc['left'] > 0 && theta_child < 0) {
            theta_child_adj <- theta_child + 2
          } else if (arc['left'] < 0 && theta_child > 0) {
            theta_child_adj <- theta_child - 2
          }
        }
        if (arc['left'] < theta_child_adj) {
          arc['left'] <- theta_child
        }
      } else if (delta_adj < 0) {
        if (abs(delta) > 1) {
          if (arc['right'] > 0 && theta_child < 0) {
            theta_child_adj <- theta_child + 2
          } else if (arc['right'] < 0 && theta_child > 0) {
            theta_child_adj <- theta_child - 2
          }
        }
        if (arc['right'] > theta_child_adj) {
          arc['right'] <- theta_child
        }
      }
    }
  }

  arc[arc < 0] <- arc[arc < 0] + 2
  arc
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
  .daylightGetArcAngles(df, origin_id, subtree, .daylightBuildCache(df))
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
  .daylightRotatePoints(df, pivot_node, nodes, angle, .daylightBuildCache(df))
}

.daylightRotatePoints <- function(df, pivot_node, nodes, angle, cache) {
  node_rows <- cache$row_index[nodes]
  node_rows <- node_rows[!is.na(node_rows)]
  if (length(node_rows) == 0L) {
    return(df)
  }

  pivot_row <- cache$row_index[pivot_node]
  pivot_x <- df$x[pivot_row]
  pivot_y <- df$y[pivot_row]
  cospitheta <- cospi(angle)
  sinpitheta <- sinpi(angle)
  delta_x <- df$x[node_rows] - pivot_x
  delta_y <- df$y[node_rows] - pivot_y

  df$x[node_rows] <- cospitheta * delta_x - sinpitheta * delta_y + pivot_x
  df$y[node_rows] <- sinpitheta * delta_x + cospitheta * delta_y + pivot_y

  tip_nodes <- nodes[cache$is_tip[nodes]]
  if (length(tip_nodes) > 0L) {
    tip_rows <- cache$row_index[tip_nodes]
    parent_rows <- cache$row_index[df$parent[tip_rows]]
    theta <- getNodeAngle.vector(df$x[parent_rows], df$y[parent_rows], df$x[tip_rows], df$y[tip_rows])
    df$angle[tip_rows] <- 180 * ifelse(theta < 0, 2 + theta, theta)
  }

  df
}
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
    .offspring.tbl_tree(df, node, self_include = TRUE)$node
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
    children_ids <- .child.tbl_tree(df, node)$node
  if (length(children_ids) == 0L) return(NULL)
  # if node leaf, return nothing.

  subtrees = tibble::tibble(
    node = children_ids,
    subtree = purrr::map(.data$node, ~getSubtree.df(df, .x))
  )
  remaining_nodes = setdiff(df$node, purrr::flatten_int(subtrees$subtree))

  # The remaining nodes that are not found in the child subtrees are the remaining subtree nodes.
  # ie, parent node and all other nodes. We don't care how they are connected, just their id.
  parent_id <- .parent.tbl_tree(df, node)$node
  # If node is not root.
  if ((length(parent_id) > 0) & (length(remaining_nodes) > 0)) {
    subtrees = tibble::add_row(subtrees, node = parent_id, subtree = list(remaining_nodes))
  }
  purrr::transpose(subtrees)
}


.getRoot.df <- function(df, node){

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

  root <- .getRoot.df(df)
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
    if (layout %in% c("equal_angle", "daylight", "ape", "tree_and_leaf")){
        res$branch.y <- with(res, (y[match(parent, node)] + y)/2)
        res$branch.y[is.na(res$branch.y)] <- 0
    }
    res$branch <- with(res, (x[match(parent, node)] + x)/2)
    if (!is.null(res[['branch.length']])) {
        res$branch.length[is.na(res$branch.length)] <- 0
    }
    res$branch[is.na(res$branch)] <- 0
    if (layout %in% c("equal_angle", "daylight", "ape", "tree_and_leaf")){
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



.as.phylo.hclust2 <- function(x, hang=0.1, ...) {
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


