## defunct since v=3.5.3

##' scale tree color by subtree (e.g., output of cutree, kmeans, or other clustering algorithm)
##'
##' 
##' @title scale_color_subtree
##' @rdname scale-color-subtree
##' @param group taxa group information
##' @return updated tree view
##' @export
##' @author Guangchuang Yu
scale_color_subtree <- function(group) {
    message("This scale function was moved to the ggtreeDendro package\n",
        "please use 'ggtreeDendro::scale_color_subtree' instead.\n")
    NULL
}

##' @rdname scale-color-subtree
##' @export
scale_colour_subtree <- scale_color_subtree


## ##' annotation taxa with images
## ##'
## ##'
## ##' @title annotation_image
## ##' @param tree_view tree view
## ##' @param img_info data.frame with first column of taxa name and second column of image names
## ##' @param width width of the image to be plotted in image
## ##' @param align logical
## ##' @param linetype line type if align = TRUE
## ##' @param linesize line size if align = TRUE
## ##' @param offset offset of image from the tree view
## ##' @return tree view
## ##' @export
## ##' @author Guangchuang Yu
## annotation_image <- function(tree_view, img_info, width=0.1, align=TRUE, linetype="dotted", linesize =1, offset=0) {
##     stop('the `annotation_image` function was deprecated...\nplease use `geom_tiplab(geom="image")`')
## }


## ##' add phylopic layer
## ##'
## ##'
## ##' @title phylopic
## ##' @param tree_view tree view
## ##' @param phylopic_id phylopic id
## ##' @param size size of phylopic to download
## ##' @param color color
## ##' @param alpha alpha
## ##' @param node selected node
## ##' @param x x position
## ##' @param y y position
## ##' @param width width of phylopic
## ##' @return phylopic layer
## ##' @export
## ##' @importFrom ggplot2 annotation_custom
## ##' @importFrom grid rasterGrob
## ##' @author Guangchuang Yu
## phylopic <- function(tree_view, phylopic_id,
##                      size=512, color="black", alpha=0.5,
##                      node=NULL, x=NULL, y=NULL, width=.1) {

##     stop('the `phylopic` function was deprecated...\nplease use `geom_tiplab(geom="phylopic")` or `geom_nodelab(geom="phylopic")`')
## }


## ##' add subview to mainview for ggplot2 objects
## ##'
## ##'
## ##' @title subview
## ##' @param mainview main view
## ##' @param subview a ggplot or grob object
## ##' @param x x position
## ##' @param y y position
## ##' @param width width of subview
## ##' @param height height of subview
## ##' @return ggplot object
## ##' @importFrom ggplot2 annotation_custom
## ##' @importFrom ggplot2 ggplotGrob
## ##' @importFrom ggplot2 ggplot_build
## ##' @export
## ##' @author Guangchuang Yu
## subview <- function(mainview, subview, x, y, width=.1, height=.1) {
##     stop("The subview function was deprecated, please use ggimage::geom_subview() instead.")
## }


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
gzoom.phylo <- function(phy, focus, subtree=FALSE, widths=c(.3, .7)) {
    .Defunct(msg =  "This function is defunct. Use treeio::tree_subset()")
    ## if (is.character(focus)) {
    ##     focus <- which(phy$tip.label %in% focus)
    ## }

    ## group_name <- "focus"
    ## phy <- gfocus(phy, focus, group_name)

    ## foc <- attr(phy, group_name)
    ## ## foc should +1 since the group index start from 0
    ## cols <- c("black", "red")[foc+1]

    ## p1 <- ggtree(phy, colour=cols)

    ## subtr <- drop.tip(phy, phy$tip.label[-focus],
    ##                   subtree=subtree, rooted=TRUE)

    ## p2 <- ggtree(subtr, colour="red") + geom_tiplab(hjust=-0.05)
    ## p2 <- p2 + xlim(0, max(p2$data$x)*1.2)
    ## multiplot(p1, p2, ncol=2, widths=widths)

    ## invisible(list(p1=p1, p2=p2))
}

gzoom.ggtree <- function(tree_view, focus, widths=c(.3, .7), xmax_adjust=0) {
    .Defunct(msg =  "This function is defunct. Use treeio::tree_subset()")
    ## node <- MRCA(tree_view, focus[1], focus[2])
    ## if (length(focus) > 2) {
    ##     for (i in 3:length(focus)) {
    ##         node <- MRCA(tree_view, focus[i], node)
    ##     }
    ## }

    ## cpos <- get_clade_position(tree_view, node)
    ## p2 <- with(cpos, tree_view+
    ##                  xlim(xmin, xmax+xmax_adjust)+
    ##                  ylim(ymin, ymax))
    ## multiplot(tree_view, p2, ncol=2, widths=widths)
    ## invisible(list(p1=tree_view, p2=p2))
}

##' @name gzoom
##' @title gzoom method
##' @rdname gzoom-methods
##' @exportMethod gzoom
##' @param xmax_adjust adjust xmax (`xlim[2]`)
##' @aliases gzoom,ggtree-method
setMethod("gzoom", signature(object="ggtree"),
          function(object, focus, widths=c(.3, .7), xmax_adjust=0) {
              gzoom.ggtree(object, focus, widths, xmax_adjust)
          })


##' zoom selected subtree
##'
##'
##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="treedata"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="phylo"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(object, focus, subtree, widths)
          })


## gfocus <- getFromNamespace("gfocus", "treeio")

##' set legend for multiple geom_hilight layers
##'
##'
##' @title set_hilight_legend
##' @param p ggtree object
##' @param color color vector
##' @param label label vector
##' @param alpha transparency of color
##' @return updated ggtree object
##' @export
##' @author Guangchuang Yu
set_hilight_legend <- function(p, color, label, alpha=1) {
    .Defunct(msg =  "This function is defunct. New version of geom_hilight can generate legend automatically")
    ## d <- data.frame(color=color, clade=label, x=0, y=1, alpha=alpha)
    ## p + geom_rect(aes_(fill=~clade, xmin=~x, xmax=~x, ymin=~y, ymax=~y), data=d, inherit.aes=F) +
    ##     guides(fill=guide_legend(override.aes=list(fill=alpha(d$color, d$alpha))))
}



##' add node label for circular layout
##'
##'
##' @title geom_nodelab2
##' @inheritParams geom_nodelab
##' @return node label layer
##' @export
##' @author Guangchuang Yu
geom_nodelab2 <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, geom = "text", hjust = 0.5, ...) {
    ## angle <- isTip <- node <- NULL
    ## m1 <- aes(subset=(!isTip & (angle < 90 | angle > 270)), angle=angle, node = node)
    ## m2 <- aes(subset=(!isTip & (angle >= 90 & angle <=270)), angle=angle+180, node = node)

    ## if (!is.null(mapping)) {
    ##     if (!is.null(mapping$subset)) {
    ##         m1 <- aes_string(angle = "angle", node = "node",
    ##                          subset = paste0(as.expression(get_aes_var(mapping, "subset")), '& (!isTip & (angle < 90 | angle > 270))'))
    ##         m2 <- aes_string(angle = "angle+180", node = "node",
    ##                          subset = paste0(as.expression(get_aes_var(mapping, "subset")), '& (!isTip & (angle >= 90 & angle <= 270))'))
    ##     }
    ##     m1 <- modifyList(mapping, m1)
    ##     m2 <- modifyList(mapping, m2)
    ## }

    ## list(geom_nodelab(m1, hjust=hjust, nudge_x = nudge_x, nudge_y = nudge_y, geom = geom, ...),
    ##      geom_nodelab(m2, hjust=1-hjust, nudge_x = nudge_x, nudge_y = nudge_y, geom = geom, ...)
    ##      )
    .Defunct(msg = "Please use `geom_nodelab()`, which also supports circular layout")
}

