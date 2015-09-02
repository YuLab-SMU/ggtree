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
    rst <- read.paml_rst(rstfile)
    mlc <- read.codeml_mlc(mlcfile)
    ## rst@tip_seq <- mlc@tip_seq
    new("codeml",
        rst = set.paml_rst_(rst),
        mlc = mlc
        )
}


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="codeml"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )


##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="codeml"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="codeml"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
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
          function(object, ...) {
              get.fields.tree(object)
          }
          )

                        
