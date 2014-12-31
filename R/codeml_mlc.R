##' read mlc file of codeml output
##'
##' 
##' @title read.codeml_mlc 
##' @param mlcfile mlc file
##' @return A \code{codeml_mlc} object
##' @export
##' @author ygc
read.codeml_mlc <- function(mlcfile) {
    tip_seq <- read.tip_seq_mlc(mlcfile)
    dNdS <- read.dnds_mlc(mlcfile)
    
    new("codeml_mlc",
        fields   = colnames(dNdS)[-c(1,2)],
        treetext = read.treetext_paml_mlc(mlcfile),
        phylo    = read.phylo_paml_mlc(mlcfile),
        dNdS     = dNdS,
        seq_type = get_seqtype(tip_seq),
        tip_seq  = tip_seq,
        mlcfile  = mlcfile)
}

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml_mlc"),
          function(object) {
              cat("'codeml_mlc' S4 object that stored information of\n\t",
                  paste0("'", object@mlcfile, "'"),
                  ".\n")
              
              cat("...@ tree\t:")
              print.phylo(get.tree(object))                  
              
              cat("\n\twith the following features available:\n")
              cat("\t", paste0("'",
                                 paste(get.fields(object), collapse="',   '"),
                                 "'"),
                  "\n")
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "codeml_mlc"),
          function(object) {
              object@fields
          })

##' @rdname plot-methods
##' @exportMethod plot
##' @param layout layout
##' @param branch.length branch length
##' @param show.tip.label logical
##' @param position one of "branch" and "node"
##' @param annotation one of get.fields(x)
##' @param ndigits round digits
setMethod("plot", signature(x = "codeml_mlc"),
          function(x, layout = "phylogram",
                   branch.length = "branch.length",
                   show.tip.label = TRUE,
                   position = "branch",
                   annotation = "dN/dS",
                   ndigits = 2,
                   ...
                   ) {
              
          p <- ggtree(x, layout=layout, branch.length=branch.length)
          if (show.tip.label) {
              p <- p + geom_tiplab()
          }
          p <- plot.codeml_mlc_(p, position, annotation, ndigits)
          p + theme_tree2()
      })
          

plot.codeml_mlc_<- function(p, position, annotation=NULL, ndigits){
    if (!is.null(annotation) && !is.na(annotation)) {
        df <- p$data
        df[, annotation] <- round(df[, annotation], ndigits)
        
        p <- p + geom_text(aes_string(x=position),
                           label = df[[annotation]],
                           size=3, vjust=-.5)
    }
    p
}

    
##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object = "codeml_mlc"),
          function(object, ...) {
              object@phylo
          }
          )



