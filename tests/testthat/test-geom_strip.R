## Rotation of the text grob whose label is `txt`.
##
## Matching on the label matters: filtering "the non-zero rotations" would miss
## a label that legitimately resolves to 0, which made an earlier version of
## this test flaky across random trees.
rot_of_label <- function(p, txt) {
    gt <- ggplot2::ggplotGrob(p)
    rots <- numeric()
    walk <- function(g) {
        if (inherits(g, "text") &&
            any(grepl(txt, as.character(g$label), fixed = TRUE))) {
            rots <<- c(rots, as.numeric(g$rot)[1])
        }
        if (!is.null(g$children)) for (ch in g$children) walk(ch)
    }
    for (g in gt$grobs) walk(g)
    rots
}

strip_tree <- function() {
    set.seed(123)
    ape::rtree(10, rooted = TRUE, tip.label = as.character(1:10))
}

test_that("geom_strip(angle = 'auto') resolves to a numeric angle", {
    # https://github.com/YuLab-SMU/ggtree/issues/629
    # "auto" used to be forwarded to `geom_text()` as a literal string; grid
    # coerced it to NA and aborted with "invalid 'rot' value".
    tree <- strip_tree()

    for (layout in c("circular", "rectangular")) {
        p <- ggtree(tree, layout = layout, branch.length = "none") +
            geom_strip(1, 2, label = "Test Label", angle = "auto")

        expect_silent(ggplot2::ggplotGrob(p))

        rot <- rot_of_label(p, "Test Label")
        expect_length(rot, 1)
        expect_true(is.finite(rot))
    }
})

test_that("geom_strip(angle = 'auto') honours horizontal = FALSE", {
    tree <- strip_tree()
    mk <- function(horizontal) {
        ggtree(tree, layout = "circular", branch.length = "none") +
            geom_strip(1, 2, label = "Test Label",
                       angle = "auto", horizontal = horizontal)
    }

    upright <- rot_of_label(mk(TRUE), "Test Label")
    rotated <- rot_of_label(mk(FALSE), "Test Label")

    expect_length(upright, 1)
    expect_length(rotated, 1)
    # `adjust_cladelabel_angle()` adds 270 to angles <= 180 when the label
    # follows the branch instead of staying horizontal
    expect_equal(rotated, upright + 270)
})

test_that("geom_strip() still accepts a numeric angle unchanged", {
    tree <- strip_tree()

    p <- ggtree(tree, layout = "circular", branch.length = "none") +
        geom_strip(1, 2, label = "Test Label", angle = 45)

    expect_equal(rot_of_label(p, "Test Label"), 45)
})
