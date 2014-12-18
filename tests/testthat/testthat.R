library(ggtree)
library(ape)

context("tree")

n = 10
tr = rtree(10)
p <- ggtree(tr)
test_that("tree record number of binary tree", {
	expect_equal(nrow(p$data), 2*n-1)
})
