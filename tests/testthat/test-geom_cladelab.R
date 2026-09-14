## Find the text grob whose (deparsed) label matches `pattern`.
cladelab_text_grob <- function(p, pattern) {
    gt <- ggplot2::ggplotGrob(p)
    found <- NULL
    walk <- function(g) {
        if (inherits(g, "text") && any(grepl(pattern, as.character(g$label)))) {
            found <<- g
        }
        if (!is.null(g$children)) for (ch in g$children) walk(ch)
    }
    for (g in gt$grobs) walk(g)
    found
}

test_that("geom_cladelab(parse = TRUE) renders the label as an expression", {
    # https://github.com/YuLab-SMU/ggtree/issues/709
    # `parse` is a parameter of the geom, not an aesthetic. It used to be
    # dropped by `build_text_layer()`, so ggplot2 fell back to the geom's own
    # `parse = FALSE` default and the label was drawn as a literal string.
    # Checking that the plot merely *builds* is not enough -- the bug was
    # silent, so assert on the rendered grob.
    set.seed(1)
    tr <- ape::rtree(5)

    p <- ggtree(tr) +
        geom_cladelab(node = 6, label = "italic('Test')", parse = TRUE) +
        xlim(c(0, 3))

    g <- cladelab_text_grob(p, "Test")
    expect_false(is.null(g))
    expect_true(is.expression(g$label))
    expect_identical(as.character(g$label), "italic(\"Test\")")
})

test_that("geom_cladelab(parse = FALSE) keeps the label as a literal string", {
    set.seed(1)
    tr <- ape::rtree(5)

    p <- ggtree(tr) +
        geom_cladelab(node = 6, label = "italic('Test')", parse = FALSE) +
        xlim(c(0, 3))

    g <- cladelab_text_grob(p, "Test")
    expect_false(is.null(g))
    expect_false(is.expression(g$label))
    expect_identical(as.character(g$label), "italic('Test')")
})

test_that("geom_cladelab(parse = TRUE) works on circular layouts too", {
    # the circular branch of `build_text_layer()` splits the label into two
    # layers (flip the text on the left half); both must inherit `parse`
    set.seed(1)
    tr <- ape::rtree(5)

    p <- ggtree(tr, layout = "circular") +
        geom_cladelab(node = 6, label = "italic('Test')", parse = TRUE)

    g <- cladelab_text_grob(p, "Test")
    expect_false(is.null(g))
    expect_true(is.expression(g$label))
})
