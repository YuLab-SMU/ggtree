context('as.polytomy')

test_that('collapse tree to polytomy', {
    file <- system.file("extdata/RAxML", "RAxML_bipartitions.H3", package="treeio")
    tree <- read.tree(file)
    cutoff <- 70
    tree2 <- as.polytomy(tree, 'node.label', function(x) as.numeric(x) < cutoff)
    expect_true(all(as.numeric(tree2$node.label) > 70, na.rm=T))
})

