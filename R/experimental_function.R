add_range <- function(p, ...) {
    df <- p$data
    rr <- gsub("\\[", "", df$height_range)
    rr <- gsub("\\]", "", rr)
    rr2 <- strsplit(rr, split=',') %>% do.call('rbind', .)
    rr2 <- matrix(as.numeric(rr2), ncol=2, byrow=FALSE)
    ## if (!is.null(mrsd)) {
    ##     mrsd %<>% as.Date
    ##     date <- Date2decimal(mrsd)
    ##     rr2 <- rr2 + date - max(rr2)
    ##     if (asDate) {
    ##         rr2 <- decimal2Date(rr2)
    ##     }
    ## }
    rr2 <- rr2 + df$x - df$height
    p + geom_segment2(x=rr2[,1], xend=rr2[,2], y=df$y, yend=df$y, ...)
}

## add range of height, only work with beast and only for height
## when I have an idea of implementing such feature for all range
## I will export it and make it usable for all users.
##
##
## file <- system.file("extdata/BEAST", "beast_mcc.tree", package="ggtree")
## beast <- read.beast(file)
## p <- ggtree(beast)
## add_range(p, color='firebrick', size=2, alpha=.3)
##
## p <- ggtree(beast, mrsd='2013-01-01') + theme_tree2()
## add_range(p, color='firebrick', size=2, alpha=.3)
