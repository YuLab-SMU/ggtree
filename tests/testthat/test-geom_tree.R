context("geom_tree")

test_that("size= is translated to linewidth= without a deprecation warning (#684)", {
    ## `size` for lines was deprecated in ggplot2 3.4.0, but `ggtree(tr, size = )`
    ## is a documented and very common idiom, so ggtree has to translate the
    ## parameter itself rather than let ggplot2 blame ggtree for the warning.
    tree <- ape::rtree(10)

    expect_no_warning(ggplot2::ggplot_build(ggtree(tree, size = 0.5)))
    expect_no_warning(ggplot2::ggplot_build(ggtree(tree, size = 0.5, ladderize = FALSE)))
    expect_no_warning(ggplot2::ggplot_build(ggtree(tree) + geom_tree(size = 2)))
    expect_no_warning(ggplot2::ggplot_build(ggtree(tree, linewidth = 0.5)))

    ## and it still controls the line width
    expect_equal(unique(ggplot_build(ggtree(tree, size = 2))$data[[1]]$linewidth), 2)
    expect_equal(unique(ggplot_build(ggtree(tree, linewidth = 2))$data[[1]]$linewidth), 2)
    expect_equal(unique(ggplot_build(ggtree(tree))$data[[1]]$linewidth), 0.5)
})

test_that("size= is translated for every layout family (#684)", {
    tree <- ape::rtree(10)
    for (layout in c("rectangular", "dendrogram", "circular", "fan",
                     "slanted", "equal_angle", "ellipse")) {
        expect_no_warning(
            ggplot2::ggplot_build(ggtree(tree, layout = layout, size = 0.5)),
            message = paste("layout:", layout)
        )
    }
})
