##' read baseml output
##'
##' 
##' @title read.codeml 
##' @param rstfile rst file
##' @param mlcfile mlc file
##' @return A \code{codeml} object
##' @export
##' @author ygc
read.codeml <- function(rstfile, mlcfile) {
    rst = read.paml_rst(rstfile)
    mlc = read.codeml_mlc(mlcfile)
    rst@tip_seq <- mlc@tip_seq
    new("codeml",
        rst = set.paml_rst_(rst),
        mlc = mlc
        )
}


##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml"),
          function(object) {
              cat("'codeml' S4 object that stored information of\n\t",
                  paste0("'", object@rst@rstfile, "' and '",
                         object@mlc@mlcfile, "'"),
                  ".\n")
              cat("...@ tree\t:")
              print.phylo(get.tree(object))                  
              cat("\n\twith the following features available:\n")
              cat("\t", paste0("'",
                               paste(get.fields(object), collapse="',   '"),
                               "'"),
                  "\n") 
              
          })


##' @rdname get.tree-methods
##' @exportMethod get.tree
##' @param by one of rst or mlc
setMethod("get.tree", signature(object="codeml"),
          function(object, by="rst", ...) {
              if (by == "rst") {
                  return(object@rst@phylo)
              } else {
                  return(object@mlc@phylo)
              }
          })

##' @rdname get.subs-methods
##' @exportMethod get.subs
setMethod("get.subs", signature(object = "codeml"),
          function(object, type, ...) {
              get.subs(object@rst, type, ...)
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="codeml"),
          function(object) {
              fields <- c(get.fields(object@rst),
                          get.fields(object@mlc))
              return(unique(fields))
          }
          )

##' @rdname plot-methods
##' @exportMethod plot
##' @importFrom ggplot2 aes_string
setMethod("plot", signature(x = "codeml"),
          function(x, layout = "phylogram",
                   branch.length = "mlc.branch.length",
                   show.tip.label = TRUE,
                   position = "branch",
                   annotation = "dN/dS",
                   ndigits = 2,
                   ...) {

              p <- ggtree(x, layout = layout, branch.length = branch.length)

              if (show.tip.label) {
                  p <- p + geom_tiplab()
              }
              
              if (!is.null(annotation) && !is.na(annotation)) {
                  if (annotation %in% get.fields(x@mlc)) {
                      p <- plot.codeml_mlc_(p, position, annotation, ndigits)
                  } else {
                      anno <- get.subs(x@rst, type=annotation)
                      p <- p %<+% anno + geom_text(aes_string(x=position,
                                                              label="subs"),
                                                   size=3, vjust=-.5)
                  }
              }
              p + theme_tree2()
          }
          )

                        
