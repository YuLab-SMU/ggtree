## get_layout() must report how the coordinates were actually produced.

test_that("get_layout() reports the layout that computed the coordinates", {
    set.seed(1)
    tr <- ape::rtree(8)
    expect_identical(get_layout(ggtree(tr)), "rectangular")
    expect_identical(get_layout(ggtree(tr, layout = "circular")), "circular")
    expect_identical(get_layout(ggtree(tr, layout = "daylight")), "daylight")
})

test_that("get_layout() follows a layout_* layer", {
    set.seed(1)
    tr <- ape::rtree(8)
    expect_identical(get_layout(ggtree(tr) + layout_dendrogram()), "dendrogram")
    expect_identical(get_layout(ggtree(tr) + layout_fan(angle = 90)), "fan")
    expect_identical(get_layout(ggtree(tr) + layout_circular()), "circular")
})

test_that("an external layout function is reported as such", {
    skip_if_not_installed("igraph")
    set.seed(1)
    tr <- ape::rtree(8)
    p <- suppressMessages(ggtree(tr, layout = igraph::layout_with_kk))
    expect_identical(get_layout(p), "external")
    expect_identical(attr(p$data, "layout"), "external")
})

test_that("get_layout() returns NULL when the layout is unknown", {
    df <- data.frame(x = 1)
    expect_null(get_layout(df))
})
