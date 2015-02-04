##' read baseml output
##'
##' 
##' @title read.codeml 
##' @param rstfile rst file
##' @param mlcfile mlc file
##' @return A \code{codeml} object
##' @export
##' @author ygc
##' @examples
##' rstfile <- system.file("extdata/PAML_Codeml", "rst", package="ggtree")
##' mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="ggtree")
##' read.codeml(rstfile, mlcfile) 
read.codeml <- function(rstfile, mlcfile) {
    rst = read.paml_rst(rstfile)
    mlc = read.codeml_mlc(mlcfile)
    rst@tip_seq <- mlc@tip_seq
    new("codeml",
        rst = set.paml_rst_(rst),
        mlc = mlc
        )
}


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml"),
          function(object, focus) {
              groupOTU_(object, focus)
          }
          )

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="codeml"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml"),
          function(object) {
              cat("'codeml' S4 object that stored information of\n\t",
                  paste0("'", object@rst@rstfile, "' and \n\t'",
                         object@mlc@mlcfile, "'."),
                  "\n\n")
              cat("...@ tree:")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              print_fields(object, len=4)
          })

##' @rdname get.tipseq-methods
##' @exportMethod get.tipseq
setMethod("get.tipseq", signature(object = "codeml"),
          function(object, ...) {
              return(object@rst@tip_seq)
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
          function(x, layout        = "phylogram",
                   branch.length    = "mlc.branch.length",
                   show.tip.label   = TRUE,
                   tip.label.size   = 4,
                   tip.label.hjust  = -0.1,
                   position         = "branch",
                   annotation       = "dN_vs_dS",
                   annotation.size  = 3,
                   annotation.color = "black",
                   ndigits          = 2,
                   ...) {

              p <- ggtree(x, layout = layout,
                          branch.length = branch.length,
                          ndigits=ndigits, ...)

              if (show.tip.label) {
                  p <- p + geom_tiplab(hjust = tip.label.hjust,
                                       size  = tip.label.size)
              }
              
              if (!is.null(annotation) && !is.na(annotation)) {
                  p <- p + geom_text(aes_string(x=position,
                                                label = annotation),
                                     size = annotation.size, vjust = -.5,
                                     color = annotation.color)
              }
              p + theme_tree2()
          }
          )

                        
