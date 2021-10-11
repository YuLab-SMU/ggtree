context('geom_hilight')

set.seed(102)
tree <- rtree(60)
dat <- data.frame(id=c(62, 88), type=c("A", "B"))

test_that('geom_hilight support gradient color', {
    p <- ggtree(tree) + 
         geom_hilight(
             data = dat,
             mapping = aes(node = id, fill = type),
             gradient = TRUE,
             alpha = 0.68
         )
    expect_true(ggplot2::is.ggplot(p))
})

test_that('geom_hilight support round rectangular layer', {
    p <- ggtree(tree) +
         geom_hilight(
             data = dat,
             mapping = aes(node = id, fill = type),
             roundrect = TRUE
         )
    expect_true(ggplot2::is.ggplot(p))
})
