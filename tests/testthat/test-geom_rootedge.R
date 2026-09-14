## Layer data of the layer that `geom_rootedge()` adds (it is always last).
rootedge_data <- function(p) {
    b <- ggplot2::ggplot_build(p)
    b$data[[length(b$data)]]
}

cladogram_inputs <- function() {
    set.seed(42)
    tree <- ape::rtree(5, rooted = TRUE)
    list(phylo = tree,
         treedata = treeio::as.treedata(tidytree::as_tibble(tree)))
}

test_that("fortify keeps branch.length under branch.length='none' for treedata", {
    # https://github.com/YuLab-SMU/ggtree/issues/648
    # `set_branch_length(., "none")` drops the edge lengths, and with them the
    # column. A plain `phylo` keeps it, so geoms that map `branch.length`
    # worked for `phylo` but not for `treedata`.
    trees <- cladogram_inputs()

    d_phylo <- ggplot2::fortify(trees$phylo, branch.length = "none")
    d_td    <- ggplot2::fortify(trees$treedata, branch.length = "none")

    expect_true("branch.length" %in% names(d_phylo))
    expect_true("branch.length" %in% names(d_td))
})

test_that("geom_rootedge() works on a treedata cladogram", {
    trees <- cladogram_inputs()

    for (rootedge in list(1, 2)) {
        p <- ggtree(trees$treedata, branch.length = "none") +
            geom_rootedge(rootedge = rootedge)
        expect_silent(ggplot2::ggplotGrob(p))

        d <- rootedge_data(p)
        # the root edge must actually be drawn, not collapse to a point
        expect_equal(d$xend, d$x - rootedge)
    }
})

test_that("geom_rootedge() without rootedge works on a treedata cladogram", {
    trees <- cladogram_inputs()

    p <- ggtree(trees$treedata, branch.length = "none") + geom_rootedge()
    expect_silent(ggplot2::ggplotGrob(p))
})

test_that("geom_rootedge() agrees between phylo and treedata cladograms", {
    trees <- cladogram_inputs()

    d_phylo <- rootedge_data(
        ggtree(trees$phylo, branch.length = "none") + geom_rootedge(rootedge = 1))
    d_td <- rootedge_data(
        ggtree(trees$treedata, branch.length = "none") + geom_rootedge(rootedge = 1))

    expect_equal(d_td$x, d_phylo$x)
    expect_equal(d_td$xend, d_phylo$xend)
})

test_that("branch.length is still honoured when it is not 'none'", {
    trees <- cladogram_inputs()

    p <- ggtree(trees$treedata) + geom_rootedge(rootedge = 1)
    expect_silent(ggplot2::ggplotGrob(p))
    expect_equal(rootedge_data(p)$xend, rootedge_data(p)$x - 1)
})
