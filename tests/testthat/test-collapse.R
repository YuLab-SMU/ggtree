context('collapse')

find_collapse_candidate <- function(plot) {
    internal_nodes <- plot$data$node[!plot$data$isTip]

    for (node in internal_nodes) {
        sp <- tidytree::offspring(plot$data, node)
        if (nrow(sp) == 0) {
            next
        }

        tip_span <- diff(range(sp$y[sp$isTip], na.rm = TRUE))
        if (tip_span > 1 && any(plot$data$isTip & plot$data$y > max(sp$y, na.rm = TRUE))) {
            return(list(node = node, sp = sp, tip_span = tip_span))
        }
    }

    stop('no suitable internal node found for collapse test')
}

test_that('collapse height gives collapsed triangles a predictable y span', {
    set.seed(1)
    tree <- ape::rtree(12)
    p <- ggtree(tree) + geom_tiplab()
    candidate <- find_collapse_candidate(p)
    upper_nodes <- p$data$node[p$data$isTip & p$data$y > max(candidate$sp$y, na.rm = TRUE)]

    p2 <- collapse(p, candidate$node, mode = 'mixed', height = 1)
    triangle <- p2$layers[[length(p2$layers)]]$data

    expect_equal(diff(range(triangle$y)), 1)

    original_y <- p$data$y[match(upper_nodes, p$data$node)]
    collapsed_y <- p2$data$y[match(upper_nodes, p2$data$node)]
    expect_equal(collapsed_y, original_y - (candidate$tip_span - 1), tolerance = 1e-8)
})

test_that('collapse height supports ggplot2::rel() scaling', {
    set.seed(1)
    tree <- ape::rtree(12)
    p <- ggtree(tree) + geom_tiplab()
    candidate <- find_collapse_candidate(p)

    p2 <- collapse(p, candidate$node, mode = 'mixed', height = ggplot2::rel(0.2))
    triangle <- p2$layers[[length(p2$layers)]]$data

    expect_equal(diff(range(triangle$y)), candidate$tip_span * 0.2, tolerance = 1e-8)
})

test_that('collapse height expands back to the original data layout', {
    set.seed(2)
    tree <- ape::rtree(14)
    p <- ggtree(tree) + geom_tiplab()
    candidate <- find_collapse_candidate(p)

    p2 <- collapse(p, candidate$node, mode = 'mixed', height = 1.5)
    p3 <- expand(p2, candidate$node)

    expect_equal(
        as.data.frame(p3$data[, c('node', 'parent', 'x', 'y', 'branch', 'angle', 'label')]),
        as.data.frame(p$data[, c('node', 'parent', 'x', 'y', 'branch', 'angle', 'label')]),
        tolerance = 1e-8
    )
})

test_that('collapse validates the height argument', {
    tree <- ape::rtree(8)
    p <- ggtree(tree)
    node <- p$data$node[!p$data$isTip][1]

    expect_error(
        collapse(p, node, mode = 'mixed', height = -1),
        '`height` must be NULL, `ggplot2::rel()` with a single non-negative value, or a single non-negative numeric value.',
        fixed = TRUE
    )

    expect_error(
        collapse(p, node, mode = 'mixed', height = structure(-0.1, class = 'rel')),
        '`height` must be NULL, `ggplot2::rel()` with a single non-negative value, or a single non-negative numeric value.',
        fixed = TRUE
    )
})
