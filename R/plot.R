## ##' @rdname plot-methods
## ##' @exportMethod plot
## ##' @param tip.label.size size of tip label
## ##' @param tip.label.hjust hjust of tip.label
## ##' @param annotation.size size of annotation
## ##' @param annotation.color color of annotation
## ##' @examples
## ##' file <- system.file("extdata/BEAST", "beast_mcc.tree", package="ggtree")
## ##' beast <- read.beast(file)
## ##' plot(beast, annotation="length_0.95_HPD", branch.length="none") + theme_tree()
## setMethod("plot", signature( x= "beast"),
##           function(x, layout = "rectangular",
##                    branch.length = "branch.length",
##                    show.tip.label = TRUE,
##                    tip.label.size = 4,
##                    tip.label.hjust = -0.1,
##                    position = "branch",
##                    annotation = "rate",
##                    ndigits = 2,
##                    annotation.size = 3,
##                    annotation.color = "black",
##                    ...) {

##               p <- ggtree(x, layout     = layout,
##                           branch.length = branch.length,
##                           ndigits       = ndigits, ...)

##               if (show.tip.label) {
##                   p <- p + geom_tiplab(hjust = tip.label.hjust,
##                                        size  = tip.label.size)
##                   offset <- ceiling(max(p$data$x)) * 0.1
##                   p <- p + xlim(-offset, max(p$data$x) + offset)
##               }
##               if (!is.null(annotation) && !is.na(annotation)) {
##                   if (position == "node") {
##                       position <- "x"
##                       vjust <- 0
##                       hjust <- -.05
##                   } else {
##                       vjust <- -.5
##                       hjust <- 0
##                   }

##                   p <- p + geom_text(aes_string(x=position,
##                                                 label=annotation),
##                                      size=annotation.size,
##                                      vjust= vjust, hjust = hjust,
##                                      color=annotation.color)
##               }
##               p + theme_tree2()
##           })


## ##' @rdname plot-methods
## ##' @exportMethod plot
## ##' @param layout layout
## ##' @param branch.length branch length
## ##' @param show.tip.label logical
## ##' @param position one of "branch" and "node"
## ##' @param annotation one of get.fields(x)
## ##' @param ndigits round digits
## setMethod("plot", signature(x = "codeml_mlc"),
##           function(x, layout        = "rectangular",
##                    branch.length    = "branch.length",
##                    show.tip.label   = TRUE,
##                    tip.label.size   = 4,
##                    tip.label.hjust  = -0.1,
##                    position         = "branch",
##                    annotation       = "dN_vs_dS",
##                    annotation.size  = 3,
##                    annotation.color = "black",
##                    ndigits          = 2,
##                    ...
##                    ) {

##               p <- ggtree(x, layout=layout,
##                           branch.length=branch.length,
##                           ndigits=ndigits, ...)

##               if (show.tip.label) {
##                   p <- p + geom_tiplab(hjust = tip.label.hjust,
##                                        size  = tip.label.size)
##               }
##               plot.codeml_mlc_(p, position, annotation,
##                                annotation.size, annotation.color)
##           })

## ##' @rdname plot-methods
## ##' @exportMethod plot
## setMethod("plot", signature( x= "r8s"),
##           function(x, layout = "rectangular",
##                    branch.length = "TREE",
##                    show.tip.label = TRUE,
##                    tip.label.size = 4,
##                    tip.label.hjust = 0,
##                    ...) {

##               p <- ggtree(x, layout     = layout,
##                           branch.length = branch.length, ...)

##               if (show.tip.label) {
##                   p <- p + geom_tiplab(hjust = tip.label.hjust,
##                                        size  = tip.label.size)
##                   offset <- ceiling(max(p$data$x)) * 0.1
##                   p <- p + xlim(NA, max(p$data$x) + offset)
##               }
##               p + theme_tree2()
##           })

## ##' @rdname plot-methods
## ##' @exportMethod plot
## setMethod("plot", signature( x= "raxml"),
##           function(x, layout = "rectangular",
##                    branch.length = "branch.length",
##                    show.tip.label = TRUE,
##                    tip.label.size = 4,
##                    tip.label.hjust = 0,
##                    position = "node",
##                    annotation = "bootstrap",
##                    ndigits = 2,
##                    annotation.size = 4,
##                    annotation.color = "black",
##                    ...) {

##               p <- ggtree(x, layout     = layout,
##                           branch.length = branch.length,
##                           ndigits       = ndigits, ...)

##               if (show.tip.label) {
##                   p <- p + geom_tiplab(hjust = tip.label.hjust,
##                                        size  = tip.label.size)
##                   offset <- ceiling(max(p$data$x)) * 0.1
##                   p <- p + xlim(NA, max(p$data$x) + offset)
##               }
##               if (!is.null(annotation) && !is.na(annotation)) {
##                   if (position == "node") {
##                       position <- "x"
##                       vjust <- 0
##                       hjust <- -.05
##                   } else {
##                       vjust <- -.5
##                       hjust <- 0
##                   }

##                   p <- p + geom_text(aes_string(x=position,
##                                                 label=annotation),
##                                      size=annotation.size,
##                                      vjust= vjust, hjust = hjust,
##                                      color=annotation.color)
##               }
##               p + theme_tree2()
##           })


## ##' @rdname plot-methods
## ##' @exportMethod plot
## setMethod("plot", signature(x = "paml_rst"),
##           function(x, layout        = "rectangular",
##                    show.tip.label   = TRUE,
##                    tip.label.size   = 4,
##                    tip.label.hjust  = -0.1,
##                    position         = "branch",
##                    annotation       = "marginal_subs",
##                    annotation.color = "black",
##                    annotation.size  = 3,
##                    ...) {
##               plot.subs(x, layout, show.tip.label,
##                         tip.label.size,
##                         tip.label.hjust,
##                         position, annotation,
##                         annotation.color,
##                         annotation.size, ...)
##           })



## ##' @rdname plot-methods
## ##' @exportMethod plot
## setMethod("plot", signature(x = "hyphy"),
##           function(x, layout        = "rectangular",
##                    show.tip.label   = TRUE,
##                    tip.label.size   = 4,
##                    tip.label.hjust  = -0.1,
##                    position         = "branch",
##                    annotation       = "subs",
##                    annotation.color = "black",
##                    annotation.size  = 3,
##                    ...) {
##               plot.subs(x, layout, show.tip.label,
##                         tip.label.size,
##                         tip.label.hjust,
##                         position, annotation,
##                         annotation.color,
##                         annotation.size,...)
##           })


## ##' @rdname plot-methods
## ##' @exportMethod plot
## ##' @importFrom ggplot2 aes_string
## setMethod("plot", signature(x = "codeml"),
##           function(x, layout        = "rectangular",
##                    branch.length    = "mlc.branch.length",
##                    show.tip.label   = TRUE,
##                    tip.label.size   = 4,
##                    tip.label.hjust  = -0.1,
##                    position         = "branch",
##                    annotation       = "dN_vs_dS",
##                    annotation.size  = 3,
##                    annotation.color = "black",
##                    ndigits          = 2,
##                    ...) {

##               p <- ggtree(x, layout = layout,
##                           branch.length = branch.length,
##                           ndigits=ndigits, ...)

##               if (show.tip.label) {
##                   p <- p + geom_tiplab(hjust = tip.label.hjust,
##                                        size  = tip.label.size)
##               }

##               if (!is.null(annotation) && !is.na(annotation)) {
##                   p <- p + geom_text(aes_string(x=position,
##                                                 label = annotation),
##                                      size = annotation.size, vjust = -.5,
##                                      color = annotation.color)
##               }
##               p + theme_tree2()
##           }
##           )

