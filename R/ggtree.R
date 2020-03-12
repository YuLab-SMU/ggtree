##' drawing phylogenetic tree from phylo object
##'
##'
##' @title ggtree
##' @inheritParams geom_tree
##' @param tr phylo object
##' @param open.angle open angle, only for 'fan' layout
##' @param mrsd most recent sampling date
##' @param as.Date logical whether using Date class in time tree
##' @param yscale y scale
##' @param yscale_mapping yscale mapping for category variable
##' @param ladderize logical (default `TRUE`). Should the tree be re-organized to have a 'ladder'
##' aspect?
##' @param right logical. If `ladderize = TRUE`, should the ladder have the smallest clade on the
##' right-hand side? See [ape::ladderize()] for more information. 
##' @param branch.length variable for scaling branch, if 'none' draw cladogram
##' @param root.position position of the root node (default = 0)
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
##' @seealso [ape::ladderize()]
##' @references 1. G Yu, TTY Lam, H Zhu, Y Guan (2018). Two methods for mapping and visualizing associated data
##' on phylogeny using ggtree. Molecular Biology and Evolution, 35(2):3041-3043.
##' <https://doi.org/10.1093/molbev/msy194>
##'
##' 2. G Yu, DK Smith, H Zhu, Y Guan, TTY Lam (2017). ggtree: an R package for
##' visualization and annotation of phylogenetic trees with their covariates and
##' other associated data. Methods in Ecology and Evolution, 8(1):28-36.
##' <https://doi.org/10.1111/2041-210X.12628>
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
                   root.position  = 0,
                   ...) {

    # Check if layout string is valid.
    layout %<>% match.arg(c("rectangular", "slanted", "fan", "circular",
                            "radial", "unrooted", "equal_angle", "daylight",
                            "ape"))

    if (layout == "unrooted") {
        layout <- "daylight"
        message('"daylight" method was used as default layout for unrooted tree.')
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
                root.position = root.position,
                ...)

    if (is(tr, "multiPhylo")) {
        multiPhylo <- TRUE
    } else {
        multiPhylo <- FALSE
    }

    p <- p + geom_tree(layout=layout, multiPhylo=multiPhylo, ...)


    p <- p + theme_tree()

    if (layout == "circular" || layout == "radial") {
        p <- p + layout_circular()
    } else if (layout == "fan") {
        p <- p + layout_fan(open.angle)
    } else if (layout %in% c("daylight", "equal_angle", "ape")) {
        p <- p + ggplot2::coord_fixed()
    } else if (yscale == "none") {
        p <- p +
            scale_y_continuous(expand = expansion(0, 0.6))
    }

    class(p) <- c("ggtree", class(p))

    return(p)
}
