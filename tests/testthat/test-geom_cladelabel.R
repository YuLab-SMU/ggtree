context('geom_cladelabel')


test_that('geom_cladelabel support parsing expression', {
    p <- ggtree(rtree(30)) + geom_cladelabel(node=40, label='paste(italic("species name"), "accession number")', parse=T)
    expect_true(is.ggplot(p))
})


