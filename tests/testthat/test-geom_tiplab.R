context("geom_tiplab")

## the `geom_segment2()` leader-line layer of `geom_tiplab(align = TRUE)`:
## the only layer carrying both an `xend` (segment) and a `label`
leader_line_layer <- function(b) {
    layers <- b$data[vapply(
        b$data,
        function(d) all(c("xend", "label") %in% names(d)),
        logical(1)
    )]
    expect_length(layers, 1)
    layers[[1]]
}

test_that("aligned tip labels render on ggplot2 >= 4.0 (#707)", {
    ## `geom_tiplab(align = TRUE)` adds a `geom_segment2()` layer for the
    ## dotted leader lines. That layer used to call ggplot2's `is.waive()`,
    ## which was removed in ggplot2 4.0, so drawing the plot failed with
    ## `could not find function "is.waive"`. ggtree now uses its own
    ## `is_waiver()` (aca415c).
    tree <- ape::rtree(10)
    p <- ggtree(tree) + geom_tiplab(align = TRUE, linetype = "dotted")

    b <- ggplot2::ggplot_build(p)
    expect_error(b, NA)
    expect_error(ggplot2::ggplot_gtable(b), NA)

    ## the leader lines are actually emitted, one per tip
    expect_equal(nrow(leader_line_layer(b)), 10)
})

test_that("aligned tip labels work together with aes(subset=) (#705, #707)", {
    tree <- ape::rtree(10)
    d <- data.frame(label = tree$tip.label, keep = rep(c(TRUE, FALSE), 5))
    tree2 <- tidytree::left_join(tree, d, by = "label")

    p <- ggtree(tree2) +
        geom_tiplab(aes(subset = keep), align = TRUE, linetype = "dotted")

    b <- ggplot2::ggplot_build(p)
    expect_error(b, NA)
    expect_error(ggplot2::ggplot_gtable(b), NA)

    expect_equal(nrow(leader_line_layer(b)), 5)
})
