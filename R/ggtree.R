##' drawing phylogenetic tree from phylo object
##'
##'
##' @title ggtree
##' @param tr phylo object
##' @param mapping aes mapping
##' @param layout one of 'rectangular', 'slanted', 'fan', 'circular', 'radial', 'equal_angle' or 'daylight'
##' @param open.angle open angle, only for 'fan' layout
##' @param mrsd most recent sampling date
##' @param as.Date logical whether using Date class in time tree
##' @param yscale y scale
##' @param yscale_mapping yscale mapping for category variable
##' @param ladderize logical
##' @param right logical
##' @param branch.length variable for scaling branch, if 'none' draw cladogram
##' @param ndigits number of digits to round numerical annotation variable
##' @param ... additional parameter
##' @return tree
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 annotate
##' @importFrom ggplot2 scale_x_reverse
##' @importFrom ggplot2 ylim
##' @importFrom ggplot2 coord_flip
##' @importFrom ggplot2 coord_polar
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr)
ggtree <- function(tr,
                   mapping        = NULL,
                   layout         = "rectangular",
                   open.angle     = 0,
                   mrsd           = NULL,
                   as.Date        = FALSE,
                   yscale         = "none",
                   yscale_mapping = NULL,
                   ladderize      = TRUE,
                   right          = FALSE,
                   branch.length  = "branch.length",
                   ndigits        = NULL,
                   ...) {

    # Check if layout string is valid.
    layout %<>% match.arg(c("rectangular", "slanted", "fan", "circular", "radial", "unrooted", "equal_angle", "daylight"))
    if (layout == "unrooted") {
        layout <- "daylight"
        message('"daylight" method was used as default layout for unrooted tree.')
    }

    if (is(tr, "r8s") && branch.length == "branch.length") {
        branch.length = "TREE"
    }

    if(yscale != "none") {
        ## for 2d tree
        layout <- "slanted"
    }

    if (is.null(mapping)) {
        mapping <- aes_(~x, ~y)
    } else {
        mapping <- modifyList(aes_(~x, ~y), mapping)
    }

    p <- ggplot(tr,
                mapping       = mapping,
                layout        = layout,
                mrsd          = mrsd,
                as.Date       = as.Date,
                yscale        = yscale,
                yscale_mapping= yscale_mapping,
                ladderize     = ladderize,
                right         = right,
                branch.length = branch.length,
                ndigits       = ndigits, ...)

    if (is(tr, "multiPhylo")) {
        multiPhylo <- TRUE
    } else {
        multiPhylo <- FALSE
    }

    p <- p + geom_tree(layout=layout, multiPhylo=multiPhylo, ...)


    p <- p + theme_tree()

    if (layout == "circular" || layout == "radial") {
        p <- layout_circular(p)
        ## refer to: https://github.com/GuangchuangYu/ggtree/issues/6
        ## and also have some space for tree scale (legend)
        p <- p + ylim(0, NA)
    } else if (layout == "fan") {
        p <- layout_fan(p, open.angle)
    }

    class(p) <- c("ggtree", class(p))

    return(p)
}
