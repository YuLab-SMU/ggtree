##' annotation taxa with images
##'
##'
##' @title annotation_image
##' @param tree_view tree view
##' @param img_info data.frame with first column of taxa name and second column of image names
##' @param width width of the image to be plotted in image
##' @param align logical
##' @param linetype line type if align = TRUE
##' @param linesize line size if align = TRUE
##' @param offset offset of image from the tree view
##' @return tree view
##' @export
##' @author Guangchuang Yu
annotation_image <- function(tree_view, img_info, width=0.1, align=TRUE, linetype="dotted", linesize =1, offset=0) {
    stop('the `annotation_image` function was deprecated...\nplease use `geom_tiplab(geom="image")`')
}


##' add phylopic layer
##'
##'
##' @title phylopic
##' @param tree_view tree view
##' @param phylopic_id phylopic id
##' @param size size of phylopic to download
##' @param color color
##' @param alpha alpha
##' @param node selected node
##' @param x x position
##' @param y y position
##' @param width width of phylopic
##' @return phylopic layer
##' @export
##' @importFrom ggplot2 annotation_custom
##' @importFrom grid rasterGrob
##' @author Guangchuang Yu
phylopic <- function(tree_view, phylopic_id,
                     size=512, color="black", alpha=0.5,
                     node=NULL, x=NULL, y=NULL, width=.1) {

    stop('the `phylopic` function was deprecated...\nplease use `geom_tiplab(geom="phylopic")` or `geom_nodelab(geom="phylopic")`')
}


##' add subview to mainview for ggplot2 objects
##'
##'
##' @title subview
##' @param mainview main view
##' @param subview a ggplot or grob object
##' @param x x position
##' @param y y position
##' @param width width of subview, [0,1]
##' @param height height of subview, [0,1]
##' @return ggplot object
##' @importFrom ggplot2 annotation_custom
##' @importFrom ggplot2 ggplotGrob
##' @importFrom ggplot2 ggplot_build
##' @export
##' @author Guangchuang Yu
subview <- function(mainview, subview, x, y, width=.1, height=.1) {
    stop("The subview function was deprecated, please use ggimage::geom_subview() instead.")
}

