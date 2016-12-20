## ##' read baseml output
## ##'
## ##' 
## ##' @title read.codeml 
## ##' @param rstfile rst file
## ##' @param mlcfile mlc file
## ##' @return A \code{codeml} object
## ##' @export
## ##' @author ygc
## ##' @examples
## ##' rstfile <- system.file("extdata/PAML_Codeml", "rst", package="ggtree")
## ##' mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="ggtree")
## ##' read.codeml(rstfile, mlcfile) 
## read.codeml <- function(rstfile, mlcfile) {
##     rst <- read.paml_rst(rstfile)
##     mlc <- read.codeml_mlc(mlcfile)
##     ## rst@tip_seq <- mlc@tip_seq
##     new("codeml",
##         rst = set.paml_rst_(rst),
##         mlc = mlc
##         )
## }




## ##' @rdname scale_color-methods
## ##' @exportMethod scale_color
## setMethod("scale_color", signature(object="codeml"),
##           function(object, by, ...) {
##               scale_color_(object, by, ...)
##           })




## ##' @rdname get.tipseq-methods
## ##' @exportMethod get.tipseq
## setMethod("get.tipseq", signature(object = "codeml"),
##           function(object, ...) {
##               return(object@rst@tip_seq)
##           })


## ##' @rdname get.subs-methods
## ##' @exportMethod get.subs
## setMethod("get.subs", signature(object = "codeml"),
##           function(object, type, ...) {
##               get.subs(object@rst, type, ...)
##           }
##           )


## ##' @rdname get.fields-methods
## ##' @exportMethod get.fields
## setMethod("get.fields", signature(object="codeml"),
##           function(object, ...) {
##               get.fields.tree(object)
##           }
##           )

                        
