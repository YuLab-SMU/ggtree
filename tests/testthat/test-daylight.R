context('daylight')

reference_daylight_layout <- data.frame(
    node = 1:19,
    parent = c(11L, 13L, 14L, 16L, 16L, 15L, 18L, 18L, 19L, 19L, 11L, 11L, 12L, 13L, 14L, 15L, 12L, 17L, 17L),
    x = c(
        0.537635332908161, -0.830129041870907, -1.93082272965061,
        -2.86040515000489, -3.10899210442627, -2.87009960010117,
        -1.31144187202577, -0.757621437012899, -0.0625367986973543,
        -0.590565454143217, 0, -0.867411764038857, -0.990520380451814,
        -1.92558918215148, -2.35350581709342, -2.66202546804589,
        -0.601581797879737, -1.05623138298823, -0.597890007101069
    ),
    y = c(
        0.157863977281575, 0.785015066442061, 0.0392283695882494,
        0.794472074756069, -0.0876386037894793, -0.985469317318234,
        -1.90676178265179, -2.23104169789429, -1.5822025523596,
        -0.944497523740547, 0, -0.254695074208212, -0.190782796865866,
        -0.0430428949334767, -0.328176394726337, -0.0892739654448295,
        -0.942722489684546, -1.61436539848238, -0.944122511145176
    ),
    angle = c(
        -73.6363636363636, 80.6657899189822, 93.6398697385642,
        102.651798162512, 179.790367121181, 231.83472519284,
        228.88477406254, 295.837415827982, 309.996863611755,
        357.069050668687, 0, -286.363636363636, 40.9090909090909,
        57.2727272727273, 73.6363636363636, 57.2727272727273,
        -400.909090909091, -368.181818181818, -466.363636363636
    )
)

test_that('optimized daylight layout matches the existing layout geometry', {
    set.seed(42)
    tree <- ape::rtree(10)

    layout <- suppressMessages(layout.unrooted(tree, layout.method = 'daylight'))
    layout_df <- as.data.frame(layout[, c('node', 'parent', 'x', 'y', 'angle')])

    expect_equal(layout_df, reference_daylight_layout, tolerance = 1e-8)
})

test_that('applyLayoutDaylight cache path matches the uncached path', {
    set.seed(7)
    tree <- ape::rtree(12)
    layout <- layoutEqualAngle(tree, 'branch.length')
    cache <- .daylightBuildCache(layout)
    node_id <- cache$internal_nodes[[2]]

    cached <- applyLayoutDaylight(layout, node_id, cache = cache)
    uncached <- applyLayoutDaylight(layout, node_id)

    expect_equal(cached$max_change, uncached$max_change, tolerance = 1e-10)
    expect_equal(
        as.data.frame(cached$tree[, c('node', 'parent', 'x', 'y', 'angle')]),
        as.data.frame(uncached$tree[, c('node', 'parent', 'x', 'y', 'angle')]),
        tolerance = 1e-10
    )
})

test_that('daylight layout preserves finite coordinates and topology', {
    set.seed(9)
    tree <- ape::rtree(25)
    equal_angle <- as.data.frame(layoutEqualAngle(tree, 'branch.length')[, c('node', 'parent')])
    daylight <- suppressMessages(as.data.frame(
        layout.unrooted(tree, layout.method = 'daylight')[, c('node', 'parent', 'x', 'y', 'angle')]
    ))

    expect_equal(daylight[, c('node', 'parent')], equal_angle)
    expect_true(all(is.finite(unlist(daylight[, c('x', 'y', 'angle')]))))
})
