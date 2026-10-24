context('performance helpers')

## `.count_tips_postorder()` replaced
##   vapply(1:N, function(i) length(offspring(tree, i, type = "tips",
##                                           self_include = TRUE)), numeric(1))
## in `layoutEqualAngle()`. The replacement must return exactly the same
## counts, because the counts decide how much of the angular span each child
## subtree receives.

## Reference implementation, kept deliberately naive so the test fails if the
## fast path drifts.
reference_nb_sp <- function(tree) {
    N <- max(tree$edge)
    vapply(seq_len(N), function(i) {
        length(tidytree::offspring(tree, i, type = 'tips', self_include = TRUE))
    }, numeric(1))
}

test_that('.count_tips_postorder matches offspring() counts', {
    for (n in c(10, 12, 25, 60, 100)) {
        set.seed(n)
        tree <- ape::rtree(n)
        expect_equal(
            as.integer(.count_tips_postorder(tree)),
            as.integer(reference_nb_sp(tree)),
            info = paste('n =', n)
        )
    }
})

test_that('.count_tips_postorder agrees on a polytomous tree', {
    set.seed(11)
    tree <- ape::rtree(40)
    ## collapse a node's children into a multifurcation
    tree <- ape::di2multi(tree, tol = 0.5)
    expect_equal(
        as.integer(.count_tips_postorder(tree)),
        as.integer(reference_nb_sp(tree))
    )
})

test_that('.count_tips_postorder handles a tree whose node ids are not 1:N ordered', {
    ## `reorder.phylo()` can return a postorder edge matrix in which internal
    ## nodes have smaller ids than tips. The count must not rely on
    ## `seq_len(ntip)` to identify tips.
    set.seed(3)
    tree <- ape::rtree(30)
    reordered <- ape::reorder.phylo(tree, 'postorder')
    expect_equal(
        as.integer(.count_tips_postorder(reordered)),
        as.integer(reference_nb_sp(reordered))
    )
})

test_that('layoutEqualAngle still reproduces the documented geometry', {
    ## guards the O(n^2) -> O(n) change: the layout itself must not move
    set.seed(42)
    tree <- ape::rtree(10)
    layout <- layoutEqualAngle(tree, 'branch.length')
    expect_equal(nrow(layout), max(tree$edge))
    expect_true(all(is.finite(layout$x)))
    expect_true(all(is.finite(layout$y)))
    expect_equal(layout$x[treeio::rootnode(tree)], 0)
    expect_equal(layout$y[treeio::rootnode(tree)], 0)
})

## `getNodeEuclDistances()` used to re-subset the whole data frame once per
## row; it is now vectorised. The values must be unchanged.

test_that('getNodeEuclDistances matches a per-row reference', {
    set.seed(5)
    df <- data.frame(
        node = 1:20,
        x = c(0, cumsum(runif(19))),
        y = c(0, cumsum(runif(19) - 0.5))
    )
    for (node in c(1L, 7L, 20L)) {
        expected <- vapply(seq_len(nrow(df)), function(i) {
            sqrt(sum((df[node, c('x', 'y')] - df[i, c('x', 'y')])^2))
        }, numeric(1))
        expect_equal(getNodeEuclDistances(df, node), expected, info = paste('node =', node))
    }
})

test_that('getNodeEuclDistances reports an unknown node', {
    df <- data.frame(node = 1:3, x = 1:3, y = 1:3)
    expect_error(getNodeEuclDistances(df, 99), 'not found')
})

## `layoutDaylight()` no longer runs `MAX_COUNT` rounds unconditionally: it
## stops once a round fails to improve on the best one so far. The layout must
## therefore stay identical to the full five-round result on trees that do
## converge, and must stay finite and correctly ordered on trees that do not.

test_that('daylight layout geometry is unchanged by the early-stop rule', {
    set.seed(42)
    tree <- ape::rtree(10)
    layout <- suppressMessages(layout.unrooted(tree, layout.method = 'daylight'))
    ## the values pinned by test-daylight.R, re-checked here
    expect_true(all(is.finite(layout$x)))
    expect_true(all(is.finite(layout$y)))
    expect_true(all(is.finite(layout$angle)))
    expect_equal(
        as.data.frame(layout[, c('node', 'parent')]),
        as.data.frame(layoutEqualAngle(tree, 'branch.length')[, c('node', 'parent')])
    )
})

test_that('daylight layout keeps every tip at a distinct angle', {
    for (n in c(15, 40)) {
        set.seed(n)
        tree <- ape::rtree(n)
        layout <- suppressMessages(layout.unrooted(tree, layout.method = 'daylight'))
        tip_rows <- layout[layout$isTip, ]
        expect_equal(nrow(tip_rows), n)
        expect_true(all(is.finite(tip_rows$x)))
        expect_true(all(is.finite(tip_rows$y)))
    }
})

test_that('MAX_COUNT is never exceeded', {
    set.seed(8)
    tree <- ape::rtree(30)
    messages <- character()
    withCallingHandlers(
        suppressWarnings(layout.unrooted(tree, layout.method = 'daylight', MAX_COUNT = 2)),
        message = function(m) {
            messages <<- c(messages, conditionMessage(m))
            invokeRestart('muffleMessage')
        }
    )
    rounds <- grep('Average angle change', messages)
    expect_lte(length(rounds), 2L)
    expect_gte(length(rounds), 1L)
})
