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
