context('tidy layout')

## The tree below is fixed so the reference coordinates do not depend on the
## version of ape used to simulate a tree.  It contains a polytomy so that the
## multifurcating code path is exercised too.
tidy_newick <- "((t1:0.1,t2:0.2,(t3:0.05,t4:0.15):0.1):0.3,((t5:0.2,t6:0.1):0.05,t7:0.4):0.2,(t8:0.15,t9:0.25,t10:0.1):0.35);"

tidy_reference <- data.frame(
    node = 1:16,
    x = c(0.40, 0.50, 0.45, 0.55, 0.45, 0.35, 0.60, 0.50, 0.60, 0.45,
          0.00, 0.30, 0.40, 0.20, 0.25, 0.35),
    y = c(6.00, 7.00, 8.00, 9.00, 1.00, 2.00, 0.00, 3.00, 4.00, 5.00,
          4.00, 7.25, 8.50, 0.75, 1.50, 4.00)
)

test_that('tidy layout reproduces the reference coordinates', {
    tr <- ape::read.tree(text = tidy_newick)
    d <- fortify(tr, layout = 'tidy')
    d <- d[order(d$node), ]

    expect_equal(d$x, tidy_reference$x, tolerance = 1e-6)
    expect_equal(d$y, tidy_reference$y, tolerance = 1e-6)
})

test_that('tidy layout keeps the tip order of the rectangular layout', {
    set.seed(20260914)
    for (i in 1:10) {
        tr <- ape::rtree(50)
        a <- fortify(tr, layout = 'rectangular')
        b <- fortify(tr, layout = 'tidy')
        expect_identical(a$node[a$isTip], b$node[b$isTip])
    }
})

test_that('tidy layout draws no branch through a node', {
    set.seed(99)
    for (i in 1:20) {
        tr <- ape::rtree(60)
        if (i %% 4 == 0) tr <- ape::di2multi(tr, tol = 0.5)
        d <- fortify(tr, layout = 'tidy')
        ii <- match(d$parent, d$node)
        ## horizontal edge of node k: (x[parent], y[k]) -> (x[k], y[k])
        hits <- vapply(seq_len(nrow(d)), function(k) {
            on_line <- which(abs(d$y - d$y[k]) < 1e-9)
            on_line <- setdiff(on_line, k)
            any(d$x[ii][on_line] < d$x[k] - 1e-9 & d$x[on_line] > d$x[k] + 1e-9)
        }, logical(1))
        expect_true(!any(hits))
    }
})

test_that('tidy layout does not overflow the node stack on deep trees', {
    ## regression: the recursive implementation failed with
    ## "node stack overflow" beyond ~1500 levels
    tr <- ape::stree(2000, type = 'left')
    tr$edge.length <- rep(1, nrow(tr$edge))
    expect_silent(res <- fortify(tr, layout = 'tidy'))
    expect_equal(nrow(res), nrow(tr$edge) + 1L)
    expect_true(!anyNA(res$y[res$isTip]))
})

test_that('tidy layout handles a cladogram without shared tip y', {
    set.seed(7)
    tr <- ape::rtree(50)
    d <- fortify(tr, layout = 'tidy', branch.length = 'none')
    ## with equal branch lengths every tip sits at the same depth, so the
    ## layout degenerates to the regular integer spacing
    expect_equal(length(unique(round(d$y[d$isTip], 9))), 50L)
    expect_identical(tip_y_alignment(ggtree(tr, layout = 'tidy',
                                            branch.length = 'none')), 'ok')
})

test_that('panel-aligned layers warn when tips are not on a regular y', {
    set.seed(7)
    tr <- ape::rtree(50)
    dat <- as.data.frame(matrix(runif(50 * 4), nrow = 50))
    rownames(dat) <- tr$tip.label

    ## the tree itself is fine -- y is unique and evenly spaced
    expect_identical(tip_y_alignment(ggtree(tr)), 'ok')

    ## under the tidy layout some tips share a y, so rows would overlap
    expect_identical(tip_y_alignment(ggtree(tr, layout = 'tidy')), 'duplicated')
    expect_warning(gheatmap(ggtree(tr, layout = 'tidy'), dat,
                            width = .5, colnames = FALSE),
                   'share the same vertical position')

    ## and there is no warning when the tips are actually aligned
    expect_warning(gheatmap(ggtree(tr), dat, width = .5, colnames = FALSE),
                   regexp = NA)
})
