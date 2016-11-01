context("xlim_expand")

test_that("dummy layer to set x axis limits of Tree panel", {
    set.seed(2016-10-31)
    tr <- rtree(50)
    tr$tip.label <- paste(tr$tip.label, tr$tip.label, sep="_")
    p <- ggtree(tr) + geom_tiplab(align=TRUE) + theme_tree2()

    d <- data.frame(id = tr$tip.label, v= rnorm(50))

    p2 <- facet_plot(p + xlim_tree(c(NA, 6)), geom=geom_point, data=d, mapping=aes(x=v), panel='dot')

    expect_true(is.ggplot(p2)) # should plot appropriately
})



test_that("dummy layer to set x axis limits of data panel", {
    set.seed(2016-10-31)
    tr <- rtree(50)
    tr$tip.label <- paste(tr$tip.label, tr$tip.label, sep="_")
    p <- ggtree(tr) + geom_tiplab(align=TRUE) + theme_tree2()

    d <- data.frame(id = tr$tip.label, v= rnorm(50))

    p2 <- facet_plot(p, geom=geom_point, data=d, mapping=aes(x=v), panel='dot') + xlim_expand(c(-4, 4), 'dot')

    expect_true(is.ggplot(p2)) # should plot appropriately
})

