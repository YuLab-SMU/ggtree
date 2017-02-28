##' plots simultaneously a whole phylogenetic tree and a portion of it.
##'
##'
##' @title gzoom
##' @param phy phylo object
##' @param focus selected tips
##' @param subtree logical
##' @param widths widths
##' @return a list of ggplot object
##' @importFrom ggplot2 xlim
##' @importFrom ggplot2 scale_color_manual
##' @author ygc
##' @examples
##' require(ape)
##' data(chiroptera)
##' gzoom(chiroptera, grep("Plecotus", chiroptera$tip.label))
gzoom.phylo <- function(phy, focus, subtree=FALSE, widths=c(.3, .7)) {
    if (is.character(focus)) {
        focus <- which(phy$tip.label %in% focus)
    }

    group_name <- "focus"
    phy <- gfocus(phy, focus, group_name)

    foc <- attr(phy, group_name)
    ## foc should +1 since the group index start from 0
    cols <- c("black", "red")[foc+1]

    p1 <- ggtree(phy, color=cols)

    subtr <- drop.tip(phy, phy$tip.label[-focus],
                      subtree=subtree, rooted=TRUE)

    p2 <- ggtree(subtr, color="red") + geom_tiplab(hjust=-0.05)
    p2 <- p2 + xlim(0, max(p2$data$x)*1.2)
    multiplot(p1, p2, ncol=2, widths=widths)

    invisible(list(p1=p1, p2=p2))
}

gzoom.ggtree <- function(tree_view, focus, widths=c(.3, .7), xmax_adjust=0) {
    node <- MRCA(tree_view, focus)
    cpos <- get_clade_position(tree_view, node)
    p2 <- with(cpos, tree_view+
                     xlim(xmin, xmax+xmax_adjust)+
                     ylim(ymin, ymax))
    multiplot(tree_view, p2, ncol=2, widths=widths)
    invisible(list(p1=tree_view, p2=p2))
}

##' @name gzoom
##' @title gzoom method
##' @rdname gzoom-methods
##' @exportMethod gzoom
##' @param xmax_adjust adjust xmax (xlim[2])
##' @aliases gzoom,ggtree-method
setMethod("gzoom", signature(object="ggtree"),
          function(object, focus, widths=c(.3, .7), xmax_adjust=0) {
              gzoom.ggtree(object, focus, widths, xmax_adjust)
          })


## ##' @rdname gzoom-methods
## ##' @exportMethod gzoom
## setMethod("gzoom", signature(object="apeBootstrap"),
##           function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
##               gzoom.phylo(get.tree(object), focus, subtree, widths)
##           })


##' zoom selected subtree
##'
##'
##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="beast"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })

##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="codeml"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="treedata"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="paml_rst"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="phylo"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(object, focus, subtree, widths)
          })


gfocus <- treeio:::gfocus
