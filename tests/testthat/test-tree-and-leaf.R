context('tree_and_leaf')

reference_tree_and_leaf_layout <- data.frame(
    node = 1:19,
    parent = c(11L, 13L, 14L, 16L, 16L, 15L, 18L, 18L, 19L, 19L, 11L, 11L, 12L, 13L, 14L, 15L, 12L, 17L, 17L),
    x = c(
        0.9450532, -0.5770846, -1.9645184, -3.3449447, -3.9294446,
        -3.4434810, -1.7004267, -0.5366381, 0.7974650, -0.4269212,
        0.0000000, -0.8712222, -0.9525181, -1.9453090, -2.4938809,
        -2.8989082, -0.5955286, -1.0582744, -0.4430016
    ),
    y = c(
        0.38773414, 1.35464495, 0.18886980, 1.58339809, 0.08799328,
        -1.46883313, -2.35422856, -3.15898716, -1.70229418, -0.89215186,
        0.00000000, -0.24971020, -0.04980296, -0.02135530, -0.42072532,
        0.09476208, -0.98087165, -1.89340151, -1.00177097
    )
)

nearest_tip_distance <- function(df) {
    tips <- as.data.frame(df[df$isTip, c('x', 'y')])
    d <- as.matrix(dist(tips))
    diag(d) <- Inf
    median(apply(d, 1, min))
}

test_that('tree_and_leaf layout is deterministic for a fixed tree', {
    set.seed(42)
    tree <- ape::rtree(10)
    layout <- suppressMessages(layout.unrooted(tree, layout.method = 'tree_and_leaf'))
    layout_df <- as.data.frame(layout[, c('node', 'parent', 'x', 'y')])

    expect_equal(layout_df, reference_tree_and_leaf_layout, tolerance = 1e-6)
})

test_that('tree_and_leaf increases median nearest-tip separation over daylight', {
    set.seed(2)
    tree <- ape::rtree(40)
    daylight <- suppressMessages(layout.unrooted(tree, layout.method = 'daylight'))
    tree_and_leaf <- suppressMessages(layout.unrooted(tree, layout.method = 'tree_and_leaf'))

    expect_gt(nearest_tip_distance(tree_and_leaf), nearest_tip_distance(daylight))
})

test_that('tree_and_leaf works through ggtree and builds cleanly', {
    set.seed(1)
    tree <- ape::rtree(20)
    plot <- ggtree(tree, layout = 'tree_and_leaf')

    expect_true(ggplot2::is_ggplot(plot))
    expect_true(all(is.finite(plot$data$x)))
    expect_true(all(is.finite(plot$data$y)))
    expect_silent(ggplot2::ggplot_build(plot))
})

test_that('tree_and_leaf accepts alternate initial layouts', {
    set.seed(3)
    tree <- ape::rtree(15)
    layout <- layout.unrooted(tree, layout.method = 'tree_and_leaf', initial_layout = 'equal_angle')

    expect_equal(nrow(layout), 2 * ape::Ntip(tree) - 1)
    expect_true(all(is.finite(layout$x)))
    expect_true(all(is.finite(layout$y)))
})
