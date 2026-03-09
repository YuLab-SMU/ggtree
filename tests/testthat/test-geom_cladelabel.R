context("geom_cladelabel")

test_that("geom_cladelabel support parsing expression", {
    tr <- ape::rtree(30)
    p0 <- ggtree(tr)
    node <- p0$data$node[which(!p0$data$isTip)[1]]
    p <- p0 + geom_cladelabel(node = node, label = 'paste(italic("species name"), "accession number")', parse = TRUE)
    expect_true(ggplot2::is_ggplot(p))
    expect_silent(ggplot2::ggplot_build(p))
})
