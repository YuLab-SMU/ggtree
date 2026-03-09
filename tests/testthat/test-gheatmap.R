context("gheatmap")

test_that("gheatmap errors when data row names do not match tree labels", {
    tree <- ape::rtree(4)
    p <- ggtree(tree)
    mat <- matrix(1:8, nrow = 4, dimnames = list(paste0("missing_", 1:4), c("a", "b")))

    expect_error(
        gheatmap(p, mat),
        "missing from `data` row names"
    )
})

test_that("gheatmap remaps named custom column labels by source column name", {
    tree <- ape::rtree(4)
    p <- ggtree(tree)
    mat <- matrix(
        1:8,
        nrow = 4,
        dimnames = list(tree$tip.label, c("A", "B"))
    )

    p2 <- gheatmap(
        p,
        mat,
        custom_column_labels = c("Renamed B" = "B", "Renamed A" = "A")
    )

    data_axis <- attr(p2, "data_axis")
    expect_equal(as.character(data_axis$custom_labels), c("Renamed A", "Renamed B"))
})

test_that("ggtree accepts layout functions returning an xy component", {
    tree <- ape::rtree(5)
    layout_fn <- function(x, ...) {
        list(xy = cbind(seq_len(9), seq_len(9)))
    }

    p <- ggtree(tree, layout = layout_fn)
    expect_true(ggplot2::is_ggplot(p))
})

