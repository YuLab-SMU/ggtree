context("gheatmap")

test_that("gheatmap warns when data row names do not match tree labels", {
    tree <- ape::rtree(4)
    p <- ggtree(tree)
    mat <- matrix(1:8, nrow = 4, dimnames = list(paste0("missing_", 1:4), c("a", "b")))

    p2 <- expect_warning(
        gheatmap(p, mat),
        "missing from `data` row names"
    )
    expect_s3_class(p2, "ggtree")
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

test_that("gheatmap does not leak a global mapping into the plot (#686)", {
    ## gheatmap used to attach the heatmap column metadata via
    ## attr(p, "mapping") <- mapping; ggplot2 >= 4.0 reads that attribute as
    ## the plot-wide aesthetic, so stat_tree() then received the 3-row
    ## heatmap mapping and failed with
    ## "Aesthetics must be either length 1 or the same as the data".
    tree <- ape::rtree(5)
    mat <- matrix(
        stats::runif(10), nrow = 5,
        dimnames = list(tree$tip.label, c("A", "B"))
    )

    p2 <- gheatmap(ggtree(tree), mat)
    ## the attribute must stay a proper mapping; a data.frame here is what
    ## made ggplot2 4.0 reject the plot with
    ## "@mapping must be <ggplot2::mapping>, not S3<data.frame>"
    expect_false(is.data.frame(attr(p2, "mapping")))
    expect_error(ggplot2::ggplot_build(p2), NA)
    expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p2)), NA)

    ## the tree layer still uses the tree data (9 nodes), not the 2 heatmap columns
    expect_equal(nrow(ggplot2::ggplot_build(p2)$data[[1]]), 9)
})

test_that("gheatmap stacks a second heatmap via ggnewscale on a circular tree (#700)", {
    testthat::skip_if_not_installed("ggnewscale")

    nwk <- system.file("extdata", "sample.nwk", package = "treeio")
    tree <- ape::read.tree(nwk)
    circ <- ggtree(tree, layout = "circular")

    df <- data.frame(
        first  = c("a", "b", "a", "c", "d", "d", "a", "b", "e", "e", "f", "c", "f"),
        second = c("z", "z", "z", "z", "y", "y", "y", "y", "x", "x", "x", "a", "a")
    )
    rownames(df) <- tree$tip.label

    df2 <- as.data.frame(matrix(stats::rnorm(39), ncol = 3))
    rownames(df2) <- tree$tip.label
    colnames(df2) <- LETTERS[1:3]

    p1 <- gheatmap(circ, df, offset = .8, width = .2,
                   colnames_angle = 95, colnames_offset_y = .25) +
        scale_fill_viridis_d(option = "D")
    p2 <- p1 + ggnewscale::new_scale_fill()
    p3 <- gheatmap(p2, df2, offset = 15, width = .3,
                   colnames_angle = 90, colnames_offset_y = .25) +
        scale_fill_viridis_c(option = "A")

    b <- ggplot2::ggplot_build(p3)
    expect_error(b, NA)
    ## 13 tips -> the tree layers hold 25 nodes each
    expect_true(all(vapply(b$data[1:2], nrow, integer(1)) == 25L))
})

test_that("ggtree accepts layout functions returning an xy component", {
    tree <- ape::rtree(5)
    layout_fn <- function(x, ...) {
        list(xy = cbind(seq_len(9), seq_len(9)))
    }

    p <- ggtree(tree, layout = layout_fn)
    expect_true(ggplot2::is_ggplot(p))
})

