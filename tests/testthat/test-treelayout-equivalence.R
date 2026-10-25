## Cross-package layout equivalence.
##
## `treelayout` was extracted from ggtree's layout code, so `fortify()` must
## keep returning exactly the same coordinates. These tests compare the two
## implementations directly rather than pinning values: the point is that the
## extraction is a move, not a rewrite.
##
## Skipped when treelayout is not installed, so ggtree still checks on its own.

skip_if_no_treelayout <- function() {
    testthat::skip_if_not_installed("treelayout")
}

.layout_cases <- list(
    list(layout = "rectangular", branch.length = "branch.length", ladderize = TRUE),
    list(layout = "rectangular", branch.length = "none",        ladderize = TRUE),
    list(layout = "rectangular", branch.length = "branch.length", ladderize = FALSE),
    list(layout = "slanted",     branch.length = "branch.length", ladderize = TRUE),
    list(layout = "slanted",     branch.length = "none",        ladderize = TRUE),
    list(layout = "circular",    branch.length = "branch.length", ladderize = TRUE),
    list(layout = "fan",         branch.length = "branch.length", ladderize = TRUE),
    list(layout = "inward_circular", branch.length = "branch.length", ladderize = TRUE),
    list(layout = "radial",      branch.length = "branch.length", ladderize = TRUE),
    list(layout = "unrooted",    branch.length = "branch.length", ladderize = TRUE),
    list(layout = "equal_angle", branch.length = "branch.length", ladderize = TRUE),
    list(layout = "daylight",    branch.length = "branch.length", ladderize = TRUE),
    list(layout = "tree_and_leaf", branch.length = "branch.length", ladderize = TRUE),
    list(layout = "dendrogram",  branch.length = "branch.length", ladderize = TRUE),
    list(layout = "ape",         branch.length = "branch.length", ladderize = TRUE),
    list(layout = "ellipse",     branch.length = "branch.length", ladderize = TRUE),
    list(layout = "roundrect",   branch.length = "branch.length", ladderize = TRUE),
    list(layout = "tidy",        branch.length = "branch.length", ladderize = TRUE)
)

quiet_layout <- function(expr) {
    suppressWarnings(suppressMessages(expr))
}

test_that("fortify() matches treelayout() for every layout", {
    skip_if_no_treelayout()
    set.seed(42)
    tr <- ape::rtree(12, tip.label = paste0("t", 1:12))

    for (case in .layout_cases) {
        label <- paste(case$layout, case$branch.length,
                       if (case$ladderize) "ladderized" else "not-ladderized")
        mine <- quiet_layout(fortify(tr, layout = case$layout,
                                     branch.length = case$branch.length,
                                     ladderize = case$ladderize))
        theirs <- quiet_layout(treelayout::tree_layout(
            tr, layout = case$layout,
            branch.length = case$branch.length,
            ladderize = case$ladderize))

        expect_equal(mine$x, theirs$x, tolerance = 1e-10, info = label)
        expect_equal(mine$y, theirs$y, tolerance = 1e-10, info = label)
        expect_equal(mine$angle, theirs$angle, tolerance = 1e-8, info = label)
        expect_equal(mine$branch, theirs$branch, tolerance = 1e-10, info = label)
    }
})

test_that("fortify() keeps the same columns as treelayout()", {
    skip_if_no_treelayout()
    set.seed(1)
    tr <- ape::rtree(8)
    mine <- quiet_layout(fortify(tr))
    theirs <- quiet_layout(treelayout::tree_layout(tr))
    expect_setequal(names(mine), names(theirs))
    expect_setequal(class(mine), class(theirs))
})

test_that("fortify() matches treelayout() on larger trees", {
    skip_if_no_treelayout()
    ## the daylight layout used to depend on the stopping rule; larger trees
    ## are where the two implementations could drift apart
    for (n in c(50, 150, 200)) {
        set.seed(n)
        tr <- ape::rtree(n)
        for (ly in c("rectangular", "daylight", "equal_angle")) {
            mine <- quiet_layout(fortify(tr, layout = ly))
            theirs <- quiet_layout(treelayout::tree_layout(tr, layout = ly))
            expect_equal(mine$x, theirs$x, tolerance = 1e-10,
                         info = paste(ly, n))
            expect_equal(mine$y, theirs$y, tolerance = 1e-10,
                         info = paste(ly, n))
        }
    }
})

test_that("multiPhylo fortify() matches treelayout()", {
    skip_if_no_treelayout()
    set.seed(5)
    trs <- ape::rmtree(3, 6)
    mine <- quiet_layout(fortify(trs))
    theirs <- quiet_layout(treelayout::tree_layout(trs))
    expect_equal(mine$x, theirs$x, tolerance = 1e-10)
    expect_equal(mine$y, theirs$y, tolerance = 1e-10)
    expect_identical(as.character(mine$.id), as.character(theirs$.id))
})

test_that("get_layout() agrees between the two implementations", {
    skip_if_no_treelayout()
    set.seed(1)
    tr <- ape::rtree(6)
    p <- quiet_layout(ggtree(tr, layout = "circular"))
    expect_identical(get_layout(p), treelayout::get_layout(p))
})
