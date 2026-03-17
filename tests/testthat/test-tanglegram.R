test_that("fortify.tanglegram merges trees and resolves links", {
    set.seed(1)
    tr1 <- ape::rtree(6)
    tr2 <- ape::rtree(6)
    tr2$tip.label <- sample(tr1$tip.label)
    assoc <- data.frame(left = tr1$tip.label, right = tr1$tip.label, stringsAsFactors = FALSE)

    tg <- as_tanglegram(tr1, y = tr2, assoc = assoc)
    df <- fortify(tg)

    expect_s3_class(df, "tbl_tree")
    expect_equal(sort(unique(df$side)), c("left", "right"))
    expect_identical(anyDuplicated(df$node), 0L)

    links <- attr(df, "tangle_assoc")
    expect_equal(nrow(links), nrow(assoc))
    expect_true(all(c("x", "y", "xend", "yend") %in% names(links)))
})

test_that("ggdoubletree builds from raw trees", {
    set.seed(2)
    tr1 <- ape::rtree(5)
    tr2 <- ape::rtree(5)
    tr2$tip.label <- sample(tr1$tip.label)
    assoc <- data.frame(left = tr1$tip.label, right = tr1$tip.label, stringsAsFactors = FALSE)

    p <- ggdoubletree(tr1, tr2, assoc)

    expect_s3_class(p, "ggplot")
    expect_s3_class(p, "ggtree")
    expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ggdoubletree replays basic layers from ggtree inputs", {
    set.seed(3)
    tr1 <- ape::rtree(5)
    tr2 <- ape::rtree(5)
    tr2$tip.label <- sample(tr1$tip.label)
    assoc <- data.frame(left = tr1$tip.label, right = tr1$tip.label, stringsAsFactors = FALSE)

    p1 <- ggtree(tr1) + geom_tiplab()
    p2 <- ggtree(tr2) + geom_tippoint()
    paired <- ggdoubletree(p1, p2, assoc)

    expect_gte(length(paired$layers), 4)
    expect_no_error(ggplot2::ggplot_build(paired))
})


test_that("tanglegram optimization reduces or preserves crossings", {
    tr_left <- ape::read.tree(text = "((a,b),(c,d));")
    tr_right <- ape::read.tree(text = "((d,c),(b,a));")
    assoc <- data.frame(
        left = c("a", "b", "c", "d"),
        right = c("a", "b", "c", "d"),
        stringsAsFactors = FALSE
    )

    plain <- fortify(as_tanglegram(tr_left, y = tr_right, assoc = assoc), optimize = FALSE)
    improved <- fortify(as_tanglegram(tr_left, y = tr_right, assoc = assoc), optimize = TRUE)

    plain_diag <- attr(plain, "tangle_optimize")
    improved_diag <- attr(improved, "tangle_optimize")

    expect_false(isTRUE(plain_diag$optimized))
    expect_true(isTRUE(improved_diag$optimized))
    expect_gte(plain_diag$before, improved_diag$after)
    expect_equal(improved_diag$after, 0L)
})

test_that("ggdoubletree optimize path builds cleanly", {
    tr_left <- ape::read.tree(text = "((a,b),(c,d));")
    tr_right <- ape::read.tree(text = "((d,c),(b,a));")
    assoc <- data.frame(
        left = c("a", "b", "c", "d"),
        right = c("a", "b", "c", "d"),
        stringsAsFactors = FALSE
    )

    p <- ggdoubletree(tr_left, tr_right, assoc, optimize = TRUE)
    expect_no_error(ggplot2::ggplot_build(p))
})


test_that("both-side optimization is available and non-regressive", {
    tr_left <- ape::read.tree(text = "(((a,b),c),(d,e));")
    tr_right <- ape::read.tree(text = "((e,d),(c,(b,a))); ")
    assoc <- data.frame(
        left = c("a", "b", "c", "d", "e"),
        right = c("a", "b", "c", "d", "e"),
        stringsAsFactors = FALSE
    )

    right_only <- fortify(
        as_tanglegram(tr_left, y = tr_right, assoc = assoc),
        optimize = TRUE,
        optimize_side = "right"
    )
    both_side <- fortify(
        as_tanglegram(tr_left, y = tr_right, assoc = assoc),
        optimize = TRUE,
        optimize_side = "both"
    )

    diag_right <- attr(right_only, "tangle_optimize")
    diag_both <- attr(both_side, "tangle_optimize")

    expect_identical(diag_both$optimize_side, "both")
    expect_gte(diag_right$after, diag_both$after)
    expect_gte(diag_both$iterations, 1L)
})

test_that("ggdoubletree supports optimize_side both", {
    tr_left <- ape::read.tree(text = "(((a,b),c),(d,e));")
    tr_right <- ape::read.tree(text = "((e,d),(c,(b,a))); ")
    assoc <- data.frame(
        left = c("a", "b", "c", "d", "e"),
        right = c("a", "b", "c", "d", "e"),
        stringsAsFactors = FALSE
    )

    p <- ggdoubletree(tr_left, tr_right, assoc, optimize = TRUE, optimize_side = "both")
    expect_no_error(ggplot2::ggplot_build(p))
})
