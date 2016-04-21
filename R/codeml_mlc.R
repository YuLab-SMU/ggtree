##' read mlc file of codeml output
##'
##' 
##' @title read.codeml_mlc 
##' @param mlcfile mlc file
##' @return A \code{codeml_mlc} object
##' @export
##' @author ygc
##' @examples
##' mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="ggtree")
##' read.codeml_mlc(mlcfile)
read.codeml_mlc <- function(mlcfile) {
    ## tip_seq <- read.tip_seq_mlc(mlcfile)
    dNdS <- read.dnds_mlc(mlcfile)
    
    new("codeml_mlc",
        fields   = colnames(dNdS)[-c(1,2)],
        treetext = read.treetext_paml_mlc(mlcfile),
        phylo    = read.phylo_paml_mlc(mlcfile),
        dNdS     = dNdS,
        ## seq_type = get_seqtype(tip_seq),
        ## tip_seq  = tip_seq,
        mlcfile  = filename(mlcfile))
}


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="codeml_mlc"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })



##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="codeml_mlc"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )


##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="codeml_mlc"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })



##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "codeml_mlc"),
          function(object) {
              get.fields.tree(object)
          })



plot.codeml_mlc_<- function(p, position, annotation=NULL,
                            annotation.size, annotation.color){

    if (!is.null(annotation) && !is.na(annotation)) {
        p <- p + geom_text(aes_string(x=position,
                                      label = annotation),
                           size=annotation.size, vjust=-.5,
                           color = annotation.color)
    }
    p + theme_tree2()
}

    
##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object = "codeml_mlc"),
          function(object, ...) {
              object@phylo
          }
          )



