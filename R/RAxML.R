##' parse RAxML bootstrapping analysis output
##'
##' 
##' @title read.raxml
##' @param file RAxML bootstrapping analysis output
##' @return raxml object
##' @export
##' @author Guangchuang Yu
read.raxml <- function(file) {
    tree.text <- readLines(file)
    tree_text <- gsub('(:[0-9\\.eE+\\-]+)\\[(\\d+)\\]', '\\@\\2\\1', tree.text)
    phylo <- read.tree(text=tree_text)
    if(any(grepl('@', phylo$node.label))) {
        bootstrap <- as.numeric(gsub("[^@]*@(\\d+)", "\\1", phylo$node.label))
        phylo$node.label %<>% gsub("@\\d+", "", .)
    }

    if (all(phylo$node.label == "")) {
        phylo$node.label <- NULL
    }

    bootstrap <- data.frame(node = Ntip(phylo) + 1:phylo$Nnode,
                            bootstrap = bootstrap)

    new("raxml",
        file      = file,
        fields    = "bootstrap",
        treetext  = tree.text,
        phylo     = phylo,
        bootstrap = bootstrap
        )
}

##' @rdname plot-methods
##' @exportMethod plot
##' @param tip.label.size size of tip label
##' @param tip.label.hjust hjust of tip.label
##' @param annotation.size size of annotation
##' @param annotation.color color of annotation
setMethod("plot", signature( x= "raxml"),
          function(x, layout = "rectangular",
                   branch.length = "branch.length",
                   show.tip.label = TRUE,
                   tip.label.size = 4,
                   tip.label.hjust = 0,
                   position = "branch",
                   annotation = "bootstrap",
                   ndigits = 2,
                   annotation.size = 3,
                   annotation.color = "black",
                   ...) {

              p <- ggtree(x, layout     = layout,
                          branch.length = branch.length,
                          ndigits       = ndigits, ...)

              if (show.tip.label) {
                  p <- p + geom_tiplab(hjust = tip.label.hjust,
                                       size  = tip.label.size)
                  offset <- ceiling(max(p$data$x)) * 0.1
                  p <- p + xlim(-offset, max(p$data$x) + offset)
              }
              if (!is.null(annotation) && !is.na(annotation)) {
                  if (position == "node") {
                      position <- "x"
                      vjust <- 0
                      hjust <- -.05
                  } else {
                      vjust <- -.5
                      hjust <- 0
                  }
                  
                  p <- p + geom_text(aes_string(x=position,
                                                label=annotation),
                                     size=annotation.size,
                                     vjust= vjust, hjust = hjust,
                                     color=annotation.color)
              }
              p + theme_tree2()
          })

##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "raxml"),
          function(object) {
              cat("'raxml' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              print_fields(object)
          })

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="raxml"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="raxml"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="raxml"),
          function(object, by="bootstrap", ...) {
              scale_color_(object, by, ...)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="raxml"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })

##' get.tree method
##'
##'
##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @aliases get.tree,raxml
##' @exportMethod get.tree
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.tree(object, ...)
setMethod("get.tree", signature(object="raxml"),
          function(object,...) {
              object@phylo
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="raxml"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )

