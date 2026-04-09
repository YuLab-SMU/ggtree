#' Create a paired tree comparison plot
#'
#' Build a tanglegram-like paired tree view from two trees, two `ggtree`
#' objects, or a `cophylo`-like object.
#'
#' @param x A tree-like object, a `ggtree` plot, a `tanglegram`, or a `cophylo` object.
#' @param y Optional second tree-like object or `ggtree` plot.
#' @param assoc Association table. It should contain `left` and `right` columns,
#'   or two unnamed columns that will be treated as `left` and `right`.
#' @param layout Tree layout for raw-tree inputs. Only `"rectangular"` is supported in v1.
#' @param ladderize logical flag passed to `fortify()` for raw-tree inputs.
#' @param mirror logical flag indicating whether the right tree should face inward.
#' @param optimize logical flag controlling whether crossing minimization is applied.
#' @param optimize_side Which side to optimize: `"right"` (default), `"left"`, or `"both"`.
#' @param gap Separation between the two trees. Values less than 1 are treated as
#'   a fraction of the wider tree.
#' @param preserve_layers logical flag indicating whether replayable layers from
#'   `ggtree` inputs should be carried over.
#' @param ... Additional arguments passed to `fortify()` for raw-tree inputs.
#' @return A `ggtree` plot.
#' @details
#' `ggdoubletree()` is the high-level entry point for paired-tree visualization in
#' `ggtree`. It supports three main workflows:
#'
#' - two raw tree-like objects plus an association table
#' - two pre-annotated `ggtree` objects plus an association table
#' - an existing `cophylo` object
#'
#' When `optimize = TRUE`, the function applies deterministic tip-order
#' optimization before building paired coordinates. In this version, only
#' rectangular paired trees are supported.
#' @export
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_curve
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 fortify
#' @importFrom treeio as.phylo
#' @importFrom ggplot2 ggplot
#' @examples
#' tr1 <- ape::rtree(5)
#' tr2 <- ape::rtree(5)
#' tr2$tip.label <- tr1$tip.label[c(1, 3, 5, 2, 4)]
#' assoc <- data.frame(left = tr1$tip.label, right = tr2$tip.label)
#'
#' ggdoubletree(tr1, tr2, assoc)
#' ggdoubletree(tr1, tr2, assoc, optimize = TRUE, optimize_side = "both")
ggdoubletree <- function(x,
                         y = NULL,
                         assoc = NULL,
                         layout = "rectangular",
                         ladderize = TRUE,
                         mirror = TRUE,
                         optimize = FALSE,
                         optimize_side = c("right", "both", "left"),
                         gap = 0.08,
                         preserve_layers = TRUE,
                         ...) {
    optimize_side <- match.arg(optimize_side)

    tgram <- as_tanglegram(
        x = x,
        y = y,
        assoc = assoc,
        layout = layout,
        ladderize = ladderize,
        preserve_layers = preserve_layers,
        ...
    )
    tgram$params$mirror <- isTRUE(mirror)
    tgram$params$gap <- gap
    tgram$params$optimize <- isTRUE(optimize)
    tgram$params$optimize_side <- optimize_side

    df <- fortify.tanglegram(
        tgram,
        layout = layout,
        ladderize = ladderize,
        gap = gap,
        mirror = mirror,
        optimize = optimize,
        optimize_side = optimize_side,
        ...
    )

    p <- ggplot(df, aes(.data$x, .data$y)) +
        geom_tree(layout = "rectangular") +
        theme_tree()

    p <- p + geom_tanglelink()
    p <- .replay_tangle_layers(p, attr(df, "tangle_layers"))
    p$plot_env$layout <- "rectangular"
    class(p) <- c("ggtree", class(p))
    p
}

new_tanglegram <- function(left,
                           right,
                           assoc,
                           rotations = NULL,
                           tip_order = NULL,
                           left_layers = NULL,
                           right_layers = NULL,
                           call = NULL,
                           params = list()) {
    assoc <- .normalize_tangle_assoc(assoc)
    structure(
        list(
            left = left,
            right = right,
            assoc = assoc,
            rotations = rotations,
            tip_order = tip_order,
            left_layers = left_layers,
            right_layers = right_layers,
            call = call,
            params = params
        ),
        class = "tanglegram"
    )
}

#' Coerce paired-tree inputs to a tanglegram
#'
#' Normalize supported paired-tree inputs into a `tanglegram` object.
#'
#' @param x A tree-like object, a `ggtree` plot, a `tanglegram`, or a `cophylo` object.
#' @param y Optional second tree-like object or `ggtree` plot.
#' @param assoc Association table. It should contain `left` and `right` columns,
#'   or two unnamed columns that will be treated as `left` and `right`.
#' @param layout Tree layout for raw-tree inputs. Only `"rectangular"` is supported in v1.
#' @param ladderize logical flag passed to `fortify()` for raw-tree inputs.
#' @param preserve_layers logical flag indicating whether replayable layers from
#'   `ggtree` inputs should be carried over.
#' @param ... Additional arguments passed to `fortify()` for raw-tree inputs.
#' @return A `tanglegram` object.
#' @name as_tanglegram
#' @export
#' @examples
#' tr1 <- ape::rtree(5)
#' tr2 <- ape::rtree(5)
#' tr2$tip.label <- tr1$tip.label[c(1, 3, 5, 2, 4)]
#' assoc <- data.frame(left = tr1$tip.label, right = tr2$tip.label)
#'
#' tg <- as_tanglegram(tr1, y = tr2, assoc = assoc)
#' df <- fortify(tg)
as_tanglegram <- function(x, ...) {
    if (inherits(x, "tanglegram")) {
        return(x)
    }
    if (inherits(x, "cophylo")) {
        return(as_tanglegram.cophylo(x, ...))
    }
    as_tanglegram.default(x, ...)
}

#' @rdname as_tanglegram
#' @export
as_tanglegram.tanglegram <- function(x, ...) {
    x
}

#' @rdname as_tanglegram
#' @export
as_tanglegram.default <- function(x,
                                  y = NULL,
                                  assoc = NULL,
                                  layout = "rectangular",
                                  ladderize = TRUE,
                                  preserve_layers = TRUE,
                                  ...) {
    if (is.null(y) || is.null(assoc)) {
        stop("For non-tanglegram inputs, both 'y' and 'assoc' are required.", call. = FALSE)
    }

    left <- .as_tangle_side(
        x,
        side = "left",
        layout = layout,
        ladderize = ladderize,
        preserve_layers = preserve_layers,
        ...
    )
    right <- .as_tangle_side(
        y,
        side = "right",
        layout = layout,
        ladderize = ladderize,
        preserve_layers = preserve_layers,
        ...
    )

    new_tanglegram(
        left = left,
        right = right,
        assoc = assoc,
        left_layers = left$layers,
        right_layers = right$layers,
        call = match.call(),
        params = list(layout = layout, preserve_layers = preserve_layers)
    )
}

#' @rdname as_tanglegram
#' @export
as_tanglegram.cophylo <- function(x,
                                  ...,
                                  layout = "rectangular",
                                  ladderize = TRUE,
                                  preserve_layers = TRUE) {
    trees <- x$trees
    if (is.null(trees) || length(trees) < 2) {
        stop("'cophylo' object must contain two trees in $trees.", call. = FALSE)
    }
    assoc <- x$assoc
    new_tanglegram(
        left = .as_tangle_side(trees[[1]], side = "left", layout = layout, ladderize = ladderize, preserve_layers = preserve_layers, ...),
        right = .as_tangle_side(trees[[2]], side = "right", layout = layout, ladderize = ladderize, preserve_layers = preserve_layers, ...),
        assoc = assoc,
        call = match.call(),
        params = list(layout = layout, preserve_layers = preserve_layers)
    )
}

.as_tangle_side <- function(x,
                            side = c("left", "right"),
                            layout = "rectangular",
                            ladderize = TRUE,
                            preserve_layers = TRUE,
                            ...) {
    side <- match.arg(side)
    if (inherits(x, c("ggtree", "ggplot"))) {
        return(.as_tangle_side_ggplot(
            x,
            side = side,
            preserve_layers = preserve_layers
        ))
    }

    .as_tangle_side_tree(
        x,
        side = side,
        layout = layout,
        ladderize = ladderize,
        ...
    )
}

.as_tangle_side_tree <- function(x,
                                 side = c("left", "right"),
                                 layout = "rectangular",
                                 ladderize = TRUE,
                                 ...) {
    side <- match.arg(side)
    if (!identical(layout, "rectangular")) {
        stop("Only 'rectangular' layout is supported for tanglegram v1.", call. = FALSE)
    }
    df <- fortify(x, layout = layout, ladderize = ladderize, ...)
    .validate_tangle_tree_data(df, what = side)
    list(
        side = side,
        data = df,
        plot = NULL,
        layers = list(),
        source_layout = layout
    )
}

.as_tangle_side_ggplot <- function(x,
                                   side = c("left", "right"),
                                   preserve_layers = TRUE) {
    side <- match.arg(side)
    layout <- tryCatch(get_layout(x), error = function(e) NULL)
    if (!is.null(layout) && !layout %in% c("rectangular", "slanted", "roundrect", "tanglegram")) {
        stop("Only rectangular-like ggtree layouts are supported for tanglegram v1.", call. = FALSE)
    }
    df <- x$data
    .validate_tangle_tree_data(df, what = side)
    layers <- list()
    if (isTRUE(preserve_layers) && length(x$layers) > 1) {
        layers <- x$layers[-1]
    }
    list(
        side = side,
        data = df,
        plot = x,
        layers = layers,
        source_layout = layout %||% "rectangular"
    )
}

.validate_tangle_tree_data <- function(df, what = "tree") {
    required_cols <- c("node", "parent", "x", "y", "label", "isTip")
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
        stop(sprintf("%s data is missing required columns: %s", what, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    invisible(df)
}

.normalize_tangle_assoc <- function(assoc) {
    if (is.null(assoc)) {
        stop("'assoc' is required for building a tanglegram.", call. = FALSE)
    }
    assoc <- as.data.frame(assoc, stringsAsFactors = FALSE)
    if (ncol(assoc) < 2) {
        stop("'assoc' must contain at least two columns.", call. = FALSE)
    }
    if (!all(c("left", "right") %in% names(assoc))) {
        names(assoc)[1:2] <- c("left", "right")
    }
    assoc
}

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

.maybe_fraction_gap <- function(gap, left_df, right_df) {
    widths <- c(
        diff(range(left_df$x, na.rm = TRUE)),
        diff(range(right_df$x, na.rm = TRUE))
    )
    if (isTRUE(gap < 1)) {
        return(max(widths, na.rm = TRUE) * gap)
    }
    gap
}

.transform_side_data <- function(df,
                                 side = c("left", "right"),
                                 left_width = 0,
                                 right_width = 0,
                                 gap = 0,
                                 node_offset = 0) {
    side <- match.arg(side)
    df <- as.data.frame(df)

    x_cols <- intersect(c("x", "xend", "xmin", "xmax", "xintercept", "branch", "branch.x"), names(df))
    if (side == "right") {
        for (col in x_cols) {
            df[[col]] <- right_width - df[[col]]
        }
        if (all(c("xmin", "xmax") %in% names(df))) {
            xmin <- pmin(df$xmin, df$xmax)
            xmax <- pmax(df$xmin, df$xmax)
            df$xmin <- xmin
            df$xmax <- xmax
        }
        shift <- left_width + gap
    } else {
        shift <- 0
    }

    shift_cols <- intersect(c("x", "xend", "xmin", "xmax", "xintercept", "branch", "branch.x"), names(df))
    for (col in shift_cols) {
        df[[col]] <- df[[col]] + shift
    }

    if (node_offset != 0) {
        for (col in intersect(c("node", "parent"), names(df))) {
            df[[col]] <- ifelse(is.na(df[[col]]), NA, df[[col]] + node_offset)
        }
    }

    df
}

.get_tangle_root <- function(df) {
    root <- df$node[df$node == df$parent]
    if (length(root) == 0) {
        root <- setdiff(df$parent, df$node)
    }
    root[[1]]
}

.tip_rank_map <- function(df) {
    tips <- df[df$isTip, c("label", "y"), drop = FALSE]
    tips <- tips[order(tips$y, tips$label), , drop = FALSE]
    stats::setNames(seq_len(nrow(tips)), tips$label)
}

.tangle_descendant_tips <- function(df) {
    parent_map <- split(df$node[df$parent != df$node], df$parent[df$parent != df$node])
    tip_lookup <- stats::setNames(as.list(df$label[df$isTip]), df$node[df$isTip])
    cache <- new.env(parent = emptyenv())

    collect <- function(node) {
        key <- as.character(node)
        if (exists(key, envir = cache, inherits = FALSE)) {
            return(get(key, envir = cache, inherits = FALSE))
        }
        if (!is.null(tip_lookup[[key]])) {
            value <- tip_lookup[[key]]
        } else {
            children <- parent_map[[key]] %||% integer(0)
            value <- unlist(lapply(children, collect), use.names = FALSE)
        }
        assign(key, value, envir = cache)
        value
    }

    nodes <- df$node
    stats::setNames(lapply(nodes, collect), nodes)
}

.optimize_side_tip_order <- function(df, assoc, side = c("left", "right"), anchor_ranks) {
    side <- match.arg(side)
    side_col <- if (identical(side, "right")) "right" else "left"
    assoc_col <- if (identical(side, "right")) "left" else "right"

    target_vals <- anchor_ranks[match(assoc[[assoc_col]], names(anchor_ranks))]
    target_map <- tapply(target_vals, assoc[[side_col]], mean, na.rm = TRUE)
    target_map <- target_map[is.finite(target_map)]

    descendants <- .tangle_descendant_tips(df)
    parent_map <- split(df$node[df$parent != df$node], df$parent[df$parent != df$node])
    current_tip_y <- stats::setNames(df$y[df$isTip], df$label[df$isTip])
    root <- .get_tangle_root(df)

    order_node <- function(node) {
        node_key <- as.character(node)
        children <- parent_map[[node_key]] %||% integer(0)
        if (length(children) == 0) {
            return(descendants[[node_key]])
        }
        child_info <- lapply(children, function(child) {
            tip_labels <- order_node(child)
            tip_targets <- unname(target_map[tip_labels])
            tip_targets <- tip_targets[is.finite(tip_targets)]
            score <- if (length(tip_targets) > 0) mean(tip_targets) else mean(current_tip_y[tip_labels], na.rm = TRUE)
            fallback <- mean(current_tip_y[tip_labels], na.rm = TRUE)
            list(labels = tip_labels, score = score, fallback = fallback)
        })
        ord <- order(
            vapply(child_info, `[[`, numeric(1), "score"),
            vapply(child_info, `[[`, numeric(1), "fallback")
        )
        unlist(lapply(child_info[ord], `[[`, "labels"), use.names = FALSE)
    }

    unique(order_node(root))
}

.apply_tip_order <- function(df, tip_order) {
    tip_order <- intersect(tip_order, df$label[df$isTip])
    existing_tips <- df$label[df$isTip]
    remaining <- existing_tips[!existing_tips %in% tip_order]
    final_order <- c(tip_order, remaining)

    tip_rows <- match(final_order, df$label)
    df$y[tip_rows] <- seq_along(final_order)
    internal_rows <- which(!df$isTip)
    if (length(internal_rows) > 0) {
        df$y[internal_rows] <- NA_real_
        df <- re_assign_ycoord_df(df, currentNode = df$node[df$isTip])
    }
    df
}

.count_tangle_inversions <- function(assoc, left_ranks, right_ranks) {
    left_vals <- left_ranks[match(assoc$left, names(left_ranks))]
    right_vals <- right_ranks[match(assoc$right, names(right_ranks))]
    keep <- is.finite(left_vals) & is.finite(right_vals)
    left_vals <- left_vals[keep]
    right_vals <- right_vals[keep]
    if (length(left_vals) < 2) {
        return(0L)
    }
    ord <- order(left_vals, right_vals)
    right_vals <- right_vals[ord]
    total <- 0L
    for (i in seq_len(length(right_vals) - 1L)) {
        total <- total + sum(right_vals[i] > right_vals[(i + 1L):length(right_vals)])
    }
    as.integer(total)
}

.optimize_tanglegram_sides <- function(left_df, right_df, assoc, optimize = FALSE, optimize_side = c("right", "both", "left"), max_iter = 10L) {
    optimize <- isTRUE(optimize)
    optimize_side <- match.arg(optimize_side)
    left_order <- names(.tip_rank_map(left_df))
    right_order <- names(.tip_rank_map(right_df))
    before <- .count_tangle_inversions(assoc, .tip_rank_map(left_df), .tip_rank_map(right_df))

    if (!optimize) {
        return(list(
            left_df = left_df,
            right_df = right_df,
            left_order = left_order,
            right_order = right_order,
            diagnostics = list(
                optimized = FALSE,
                optimize_side = optimize_side,
                iterations = 0L,
                before = before,
                after = before
            )
        ))
    }

    left_df2 <- left_df
    right_df2 <- right_df
    current <- before
    iterations <- 0L

    optimize_one <- function(target_df, anchor_df, side_name) {
        anchor_ranks <- .tip_rank_map(anchor_df)
        proposed_order <- .optimize_side_tip_order(target_df, assoc, side = side_name, anchor_ranks = anchor_ranks)
        proposed_df <- .apply_tip_order(target_df, proposed_order)
        list(df = proposed_df, order = proposed_order)
    }

    repeat {
        iterations <- iterations + 1L
        improved <- FALSE

        if (optimize_side %in% c("right", "both")) {
            step <- optimize_one(right_df2, left_df2, "right")
            candidate <- .count_tangle_inversions(assoc, .tip_rank_map(left_df2), .tip_rank_map(step$df))
            if (candidate < current) {
                right_df2 <- step$df
                current <- candidate
                improved <- TRUE
            }
        }

        if (optimize_side %in% c("left", "both")) {
            step <- optimize_one(left_df2, right_df2, "left")
            candidate <- .count_tangle_inversions(assoc, .tip_rank_map(step$df), .tip_rank_map(right_df2))
            if (candidate < current) {
                left_df2 <- step$df
                current <- candidate
                improved <- TRUE
            }
        }

        if (!improved || iterations >= max_iter) {
            break
        }
    }

    list(
        left_df = left_df2,
        right_df = right_df2,
        left_order = names(.tip_rank_map(left_df2)),
        right_order = names(.tip_rank_map(right_df2)),
        diagnostics = list(
            optimized = TRUE,
            optimize_side = optimize_side,
            iterations = iterations,
            before = before,
            after = current
        )
    )
}

.resolve_assoc_endpoints <- function(df, assoc) {
    tip_df <- df[df$isTip, , drop = FALSE]
    left_df <- tip_df[tip_df$side == "left", , drop = FALSE]
    right_df <- tip_df[tip_df$side == "right", , drop = FALSE]

    left_idx <- match(assoc$left, left_df$label)
    right_idx <- match(assoc$right, right_df$label)
    if (anyNA(left_idx) || anyNA(right_idx)) {
        missing_left <- assoc$left[is.na(left_idx)]
        missing_right <- assoc$right[is.na(right_idx)]
        msg <- c()
        if (length(missing_left) > 0) {
            msg <- c(msg, paste("left:", paste(unique(missing_left), collapse = ", ")))
        }
        if (length(missing_right) > 0) {
            msg <- c(msg, paste("right:", paste(unique(missing_right), collapse = ", ")))
        }
        stop(sprintf("Association labels cannot be found in tree tips (%s).", paste(msg, collapse = "; ")), call. = FALSE)
    }

    cbind(
        assoc,
        left_node = left_df$node[left_idx],
        right_node = right_df$node[right_idx],
        x = left_df$x[left_idx],
        y = left_df$y[left_idx],
        xend = right_df$x[right_idx],
        yend = right_df$y[right_idx]
    )
}

#' @method fortify tanglegram
#' @export
fortify.tanglegram <- function(model,
                               data,
                               layout = "rectangular",
                               ladderize = TRUE,
                               gap = 0.08,
                               mirror = TRUE,
                               optimize = isTRUE(model$params$optimize),
                               optimize_side = model$params$optimize_side %||% "right",
                               ...) {
    if (!identical(layout, "rectangular")) {
        stop("Only 'rectangular' layout is supported for tanglegram v1.", call. = FALSE)
    }

    left_df <- model$left$data
    right_df <- model$right$data

    .validate_tangle_tree_data(left_df, "left")
    .validate_tangle_tree_data(right_df, "right")

    optimized <- .optimize_tanglegram_sides(
        left_df,
        right_df,
        model$assoc,
        optimize = optimize,
        optimize_side = optimize_side
    )
    left_df <- optimized$left_df
    right_df <- optimized$right_df

    left_width <- diff(range(left_df$x, na.rm = TRUE))
    right_width <- diff(range(right_df$x, na.rm = TRUE))
    gap_value <- .maybe_fraction_gap(gap, left_df, right_df)

    left_plot_df <- .transform_side_data(left_df, side = "left", left_width = left_width, right_width = right_width, gap = gap_value, node_offset = 0)
    right_offset <- max(left_plot_df$node, na.rm = TRUE)
    right_plot_df <- .transform_side_data(right_df, side = if (isTRUE(mirror)) "right" else "left", left_width = left_width, right_width = right_width, gap = gap_value, node_offset = right_offset)
    if (!isTRUE(mirror)) {
        right_plot_df$x <- right_plot_df$x + left_width + gap_value
        if ("branch" %in% names(right_plot_df)) {
            right_plot_df$branch <- right_plot_df$branch + left_width + gap_value
        }
        if ("branch.x" %in% names(right_plot_df)) {
            right_plot_df$branch.x <- right_plot_df$branch.x + left_width + gap_value
        }
        if ("xend" %in% names(right_plot_df)) {
            right_plot_df$xend <- right_plot_df$xend + left_width + gap_value
        }
        if ("xmin" %in% names(right_plot_df)) {
            right_plot_df$xmin <- right_plot_df$xmin + left_width + gap_value
        }
        if ("xmax" %in% names(right_plot_df)) {
            right_plot_df$xmax <- right_plot_df$xmax + left_width + gap_value
        }
    }

    left_plot_df$side <- "left"
    right_plot_df$side <- "right"
    left_plot_df$tree_id <- "left"
    right_plot_df$tree_id <- "right"
    left_plot_df$orig_node <- left_df$node
    right_plot_df$orig_node <- right_df$node

    res <- rbind(left_plot_df, right_plot_df)
    res <- calculate_branch_mid(res, layout = "rectangular")
    res <- calculate_angle(res)
    class(res) <- c("tbl_tree", class(res))

    assoc_resolved <- .resolve_assoc_endpoints(res, model$assoc)

    attr(res, "layout") <- "tanglegram"
    attr(res, "tangle_assoc") <- assoc_resolved
    attr(res, "tangle_gap") <- gap_value
    attr(res, "tangle_optimize") <- optimized$diagnostics
    attr(res, "tangle_layers") <- list(
        left = .build_replay_layers(model$left$layers, left_plot_df, side = "left", left_width = left_width, right_width = right_width, gap = gap_value, node_offset = 0),
        right = .build_replay_layers(model$right$layers, right_plot_df, side = if (isTRUE(mirror)) "right" else "left", left_width = left_width, right_width = right_width, gap = gap_value, node_offset = right_offset)
    )
    res
}

#' @method fortify cophylo
#' @export
fortify.cophylo <- function(model,
                            data,
                            layout = "rectangular",
                            ladderize = TRUE,
                            gap = 0.08,
                            mirror = TRUE,
                            ...) {
    fortify(
        as_tanglegram(model, layout = layout, ladderize = ladderize, ...),
        layout = layout,
        ladderize = ladderize,
        gap = gap,
        mirror = mirror,
        ...
    )
}

.build_replay_layers <- function(layers,
                                 transformed_tree_data,
                                 side = c("left", "right"),
                                 left_width = 0,
                                 right_width = 0,
                                 gap = 0,
                                 node_offset = 0) {
    side <- match.arg(side)
    if (length(layers) == 0) {
        return(list())
    }

    lapply(layers, function(layer) {
        new_layer <- layer
        layer_data <- layer$data
        if (inherits(layer_data, "waiver") || is.null(layer_data)) {
            new_layer$data <- transformed_tree_data
            return(new_layer)
        }
        if (!is.data.frame(layer_data)) {
            return(NULL)
        }
        new_layer$data <- .transform_side_data(
            layer_data,
            side = side,
            left_width = left_width,
            right_width = right_width,
            gap = gap,
            node_offset = node_offset
        )
        new_layer
    })
}

.replay_tangle_layers <- function(plot, layer_spec) {
    if (is.null(layer_spec)) {
        return(plot)
    }
    replay_layers <- c(layer_spec$left, layer_spec$right)
    replay_layers <- Filter(Negate(is.null), replay_layers)
    if (length(replay_layers) == 0) {
        return(plot)
    }
    for (layer in replay_layers) {
        plot <- ggplot2::ggplot_add(layer, plot, deparse(substitute(layer)))
    }
    plot
}

#' Link matching taxa across a paired tree view
#'
#' @param data Optional data. If omitted, `geom_tanglelink()` uses the resolved
#'   association table stored on the fortified tanglegram data.
#' @param mapping Optional aesthetics mapping for `left` and `right` columns.
#' @param left,right Optional vectors of left/right labels.
#' @param geom One of `"curve"` or `"segment"`.
#' @param curvature Curvature for curve links.
#' @param ... Additional parameters passed to the underlying `ggplot2` geom.
#' @return A list object that is added to a `ggplot` via `ggplot_add()`.
#' @details
#' `geom_tanglelink()` is the dedicated cross-tree link layer for
#' `ggdoubletree()` and fortified `tanglegram` objects. It can either use the
#' association metadata attached by `fortify.tanglegram()` or resolve explicit
#' `left`/`right` mappings supplied by the user.
#' @export
geom_tanglelink <- function(data = NULL,
                            mapping = NULL,
                            left = NULL,
                            right = NULL,
                            geom = c("curve", "segment"),
                            curvature = 0.15,
                            ...) {
    geom <- match.arg(geom)
    structure(
        list(
            data = data,
            mapping = mapping,
            left = left,
            right = right,
            geom = geom,
            curvature = curvature,
            params = list(...)
        ),
        class = "tanglelink"
    )
}
