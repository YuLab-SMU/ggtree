library(ggtree)
library(ape)

context("tree")

n = 10
tr = rtree(10)
p <- ggtree(tr)
test_that("tree record number of binary tree", {
	expect_equal(nrow(p$data), 2*n-1)
})


polytomy <- read.tree(text="((A, B, C), D);")
bt <- as.binary(polytomy)
test_that("convert polytomy to binary tree", {
    expect_equal(is.binary.tree(bt), TRUE)
})

df <- fortify(rtree(10))
child <- ggtree:::getChild.df(df, 11)
test_that("root node should not be included in its ancestor node list", {
    expect_equal(11 %in% child, FALSE)
})

