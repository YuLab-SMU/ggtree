context('groupOTU')

test_that('groupOTU', {
    nwk <- system.file("extdata", "sample.nwk", package="treeio")
    tree <- read.tree(nwk)
    focus <- c("D", "E", "F", "G")
    df <- fortify(groupOTU(tree, focus=focus))
    expect_true(all(df$group[df$label %in% focus] == 1))

    cls <- list(c1=c("A", "B", "C", "D", "E"),
                c2=c("F", "G", "H"),
                c3=c("L", "K", "I", "J"),
                c4="M")
    df <- fortify(groupOTU(tree, cls))
    for (i in seq_along(cls)) {
        expect_true(all(df$group[df$label %in% cls[[i]]] == names(cls)[i]))
    }
})


context('groupClade')

test_that('groupClade', {
    nwk <- system.file("extdata", "sample.nwk", package="treeio")
    tree <- read.tree(nwk)
    focus <- c("D", "E", "F", "G")
    nodes <- c(21, 17)
    df <- fortify(groupClade(tree, node=nodes))

    for (i in seq_along(nodes)) {
        expect_true(all(df$group[df$node %in% ggtree:::get.offspring.df(df, nodes[i])] == i))
    }

})

