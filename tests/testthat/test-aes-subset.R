context('aes(subset)')

## regression tests for #705 and #699
## `aes(subset = )` used to be deparsed and re-parsed via `aes_string()`,
## which dropped the quotes around character literals, so that
## `aes(subset = group == "a")` failed with `object 'a' not found`.
## fixed in 1a116a8 ("using aes instead of aes_string"); these tests lock it in.

library(ape)
library(tidytree)

set.seed(1)
tr <- rtree(10)
tree_info <- data.frame(
    label           = paste0("t", 1:10),
    group_character = c(rep("group1", 3), rep("group2", 4), rep("group3", 3))
)
tree_info$group_factor  <- factor(tree_info$group_character)
tree_info$group_integer <- as.integer(gsub("group", "", tree_info$group_character))
tree_info$in_group2     <- tree_info$group_character == "group2"
tr_plus_info <- left_join(tr, tree_info, by = "label")

## number of rows in the (single) layer that carries a `subset` aesthetic
n_subsetted <- function(p) {
    b <- ggplot2::ggplot_build(p)
    layers <- b$data[vapply(b$data, function(d) "subset" %in% names(d), logical(1))]
    expect_length(layers, 1)
    nrow(layers[[1]])
}

test_that("aes(subset=) accepts string literals (#705)", {
    ## character comparison -- the original failure: object 'group2' not found
    p <- ggtree(tr_plus_info) +
        geom_tippoint(aes(subset = (group_character == "group2")), size = 3)
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)

    p <- ggtree(tr_plus_info) +
        geom_tiplab(aes(subset = (group_character == "group2")))
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)
})

test_that("aes(subset=) accepts factor comparison (#705)", {
    p <- ggtree(tr_plus_info) +
        geom_tippoint(aes(subset = (group_factor == "group2")), size = 3)
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)
})

test_that("aes(subset=) still works for logical columns and numeric tests (#705)", {
    p <- ggtree(tr_plus_info) + geom_tippoint(aes(subset = in_group2), size = 3)
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)

    p <- ggtree(tr_plus_info) +
        geom_tippoint(aes(subset = (group_integer == 2)), size = 3)
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)
})

test_that("aes(subset=) resolves variables from the calling frame (#705)", {
    ## helper variable defined in a *local* scope: it is neither a column of
    ## the plot data nor reachable from globalenv, so the subset quosure must
    ## keep the environment of the original `aes()` call
    group_of_interest <- "group2"
    p <- ggtree(tr_plus_info) +
        geom_tippoint(aes(subset = (group_character == group_of_interest)), size = 3)
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)

    p <- ggtree(tr_plus_info) +
        geom_tiplab(aes(subset = (group_character == group_of_interest)))
    expect_error(ggplot_build(p), NA)
    expect_equal(n_subsetted(p), 4)

    ## same for a nested function scope
    f <- function() {
        wanted <- "group3"
        p <- ggtree(tr_plus_info) +
            geom_tippoint(aes(subset = (group_character == wanted)), size = 3)
        n_subsetted(p)
    }
    expect_equal(f(), 3)
})

test_that("aes(subset=) subsets to tips only for treedataList + facet_wrap (#699)", {
    set.seed(2020)
    x <- rtree(30)
    d <- data.frame(label = x$tip.label, var1 = abs(rnorm(30)), var2 = abs(rnorm(30)))
    tree <- full_join(x, d, by = "label")
    trs <- list(TREE1 = tree, TREE2 = tree)
    class(trs) <- "treedataList"

    p <- ggtree(trs) + facet_wrap(~.id) +
        geom_tippoint(aes(subset = .id == "TREE1", colour = var1)) +
        scale_colour_gradient(low = "blue", high = "red") +
        ggnewscale::new_scale_colour() +
        geom_tippoint(aes(colour = var2), data = td_filter(.id == "TREE2")) +
        scale_colour_viridis_c()

    b <- ggplot_build(p)
    expect_error(b, NA)

    ## the two geom_tippoint layers: 30 tips each, never 59 nodes
    point_layers <- b$data[!vapply(b$data, function(d) is.null(d$shape), logical(1))]
    expect_length(point_layers, 2)
    expect_true(all(vapply(point_layers, nrow, integer(1)) == 30L))
})
